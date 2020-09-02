{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Logger
    ( LogLevel(..)
    , Handle(..)
    , loggerFilter
    , withNullLogger
    , withFileLogger
    , withStdLogger
    , withMultiLogger
    , debug
    , info
    , warn
    , err
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


debug :: Handle -> Text -> IO ()
debug h = send h Debug


info :: Handle -> Text -> IO ()
info h = send h Info


warn :: Handle -> Text -> IO ()
warn h = send h Warning


err :: Handle -> Text -> IO ()
err h = send h Error


{--}


loggerFilter :: LogLevel -> Handle -> Handle
loggerFilter minlevel inner = do
    Handle
        { send = \level text -> do
            when (level >= minlevel) $ send inner level text }


{--}


withNullLogger :: (Handle -> IO r) -> IO r
withNullLogger body = do
    body $ Handle
        { send = \level text -> return () }


{--}


withFileLogger :: FilePath -> (Handle -> IO r) -> IO r
withFileLogger path body = do
    bracket
        (new path)
        fst
        (body . snd)


new :: FilePath -> IO (IO (), Handle)
new path = do
    fh <- IO.openFile path IO.AppendMode
    return $ (,) (IO.hClose fh) $ Handle
        { send = \level text -> do
            TextIO.hPutStrLn fh $ pack (show level) <> ": " <> text }


{--}


withStdLogger :: (Handle -> IO r) -> IO r
withStdLogger body = do
    body $ Handle
        { send = \level text -> do
            TextIO.putStrLn $ pack (show level) <> ": " <> text }


{--}


withMultiLogger :: Handle -> Handle -> (Handle -> IO r) -> IO r
withMultiLogger a b body = do
    body $ Handle
        { send = \level text -> do
            send a level text
            send b level text }
