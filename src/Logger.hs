{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Logger
    ( LogLevel(..)
    , Handle(..)
    , withFileLogger
    , withStdLogger
    , withMultiLogger
    ) where


import Control.Exception
import Control.Monad
import Data.Text
import qualified Data.Text.IO as TextIO
import qualified System.IO as IO


data Handle
    = Handle
        { send :: !(LogLevel -> Text -> IO ()) }


data LogLevel
    = Debug
    | Info
    | Warning
    | Error
    | Topmost
    deriving (Ord, Eq, Show)


{--}


withFileLogger :: FilePath -> LogLevel -> (Handle -> IO r) -> IO r
withFileLogger path minlevel body = do
    bracket
        (new path minlevel)
        fst
        (body . snd)


new :: FilePath -> LogLevel -> IO (IO (), Handle)
new path minlevel = do
    fh <- IO.openFile path IO.AppendMode
    return $ (,) (IO.hClose fh) $ Handle
        { send = \level text -> do
            when (level >= minlevel) $ do
                TextIO.hPutStrLn fh $ pack (show level) <> ": " <> text }


{--}


withStdLogger :: LogLevel -> (Handle -> IO r) -> IO r
withStdLogger minlevel body = do
    body $ Handle
        { send = \level text -> do
            when (level >= minlevel) $ do
                TextIO.putStrLn $ pack (show level) <> ": " <> text }


{--}


withMultiLogger :: Handle -> Handle -> (Handle -> IO r) -> IO r
withMultiLogger a b body = do
    body $ Handle
        { send = \level text -> do
            send a level text
            send b level text }
