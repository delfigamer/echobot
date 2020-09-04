{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Responder
    ( Config(..)
    , Handle(..)
    , withEchoResponder
    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.IORef
import Network.HTTP.Req
import qualified Data.HashTable.IO as IOTable
import qualified Data.Text as Text
import qualified Channel
import qualified Logger


data Config
    = Config
        { cDefaultMultiplier :: Int
        , cUnknownCommandMsg :: Text.Text
        , cInspectMultiplierCmd :: Text.Text
        , cInspectMultiplierMsg :: Text.Text
        , cMultiplierSetMsg :: Text.Text
        , cMaxMultiplier :: Int }


data Handle
    = Handle
        { work :: IO () }


withEchoResponder :: Config -> Logger.Handle -> Channel.Handle -> (Handle -> IO r) -> IO r
withEchoResponder conf logger channel body = do
    er <- erNew conf logger channel
    body $ Handle
        { work = erWork er }


data EchoResponder
    = EchoResponder
        { erConfig :: Config
        , erLogger :: Logger.Handle
        , erChannel :: Channel.Handle
        , erContextTable :: IOTable.CuckooHashTable Channel.ChatId ChatContext }


data ChatContext
    = ChatContext
        { ccMultiplier :: IORef (Maybe Int) }


erNew :: Config -> Logger.Handle -> Channel.Handle -> IO EchoResponder
erNew conf logger channel = do
    ctab <- IOTable.new
    return $ EchoResponder
        { erConfig = conf
        , erLogger = logger
        , erChannel = channel
        , erContextTable = ctab }


erWork :: EchoResponder -> IO ()
erWork er = do
    events <- Channel.poll (erChannel er)
    mapM_ (erHandleEvent er) events


erHandleEvent :: EchoResponder -> Channel.Event -> IO ()
erHandleEvent er (Channel.EventMessage chatId _ text) = do
    if Text.isPrefixOf "/" text
        then erHandleCommand er chatId text
        else erRepeatMessage er chatId text
erHandleEvent er (Channel.EventSticker chatId sticker) = do
    erRepeatSticker er chatId sticker
erHandleEvent er (Channel.EventQuery chatId messageId queryId userdata) = do
    () <$ Channel.answerQuery (erChannel er) queryId ""
    case Text.stripPrefix "r" userdata of
        Just mt
            | (mult, _):_ <- reads (Text.unpack mt)
            , 1 <= mult && mult <= cMaxMultiplier (erConfig er) -> do
                erSetMultiplier er chatId mult
                () <$ Channel.updateMessage (erChannel er) chatId messageId
                    (substitute [show mult] $ cMultiplierSetMsg (erConfig er))
                    []
        _ -> return ()


erRepeatMessage :: EchoResponder -> Channel.ChatId -> Text.Text -> IO ()
erRepeatMessage er chatId text = do
    mult <- erGetMultiplier er chatId
    replicateM_ mult $ do
        () <$ Channel.sendMessage (erChannel er) chatId text []


erRepeatSticker :: EchoResponder -> Channel.ChatId -> Channel.StickerName -> IO ()
erRepeatSticker er chatId sticker = do
    mult <- erGetMultiplier er chatId
    replicateM_ mult $ do
        () <$ Channel.sendSticker (erChannel er) chatId sticker


erHandleCommand :: EchoResponder -> Channel.ChatId -> Text.Text -> IO ()
erHandleCommand er chatId cmd
    | Text.isPrefixOf (cInspectMultiplierCmd (erConfig er)) cmd = do
        mult <- erGetMultiplier er chatId
        () <$ Channel.sendMessage (erChannel er) chatId
            (substitute [show mult] $ cInspectMultiplierMsg (erConfig er))
            multiplierButtons
    | otherwise = do
        () <$ Channel.sendMessage (erChannel er) chatId
            (cUnknownCommandMsg (erConfig er))
            []
    where
    multiplierButtons = do
        map
            (\i -> Channel.QueryButton (Text.pack (show i)) (Text.pack ("r" ++ show i)))
            [1 .. cMaxMultiplier (erConfig er)]


erGetMultiplier :: EchoResponder -> Channel.ChatId -> IO Int
erGetMultiplier er chatId = do
    mcc <- IOTable.lookup (erContextTable er) chatId
    case mcc of
        Nothing -> return $ cDefaultMultiplier (erConfig er)
        Just cc -> do
            mmult <- readIORef (ccMultiplier cc)
            case mmult of
                Nothing -> return $ cDefaultMultiplier (erConfig er)
                Just mult -> return mult


erSetMultiplier :: EchoResponder -> Channel.ChatId -> Int -> IO ()
erSetMultiplier er chatId mult = do
    cc <- erProvideChatContext er chatId
    writeIORef (ccMultiplier cc) $! Just mult


erProvideChatContext :: EchoResponder -> Channel.ChatId -> IO ChatContext
erProvideChatContext er chatId = do
    mcc <- IOTable.lookup (erContextTable er) chatId
    case mcc of
        Nothing -> do
            cc <- ChatContext
                <$> newIORef Nothing
            IOTable.insert (erContextTable er) chatId cc
            return $ cc
        Just cc -> do
            return $ cc


substitute :: [String] -> Text.Text -> Text.Text
substitute arglist format = do
    let first:rest = Text.splitOn "%" format
    Text.concat $ first:walk rest
    where
    walk ("":p:ps) = "%":p:walk ps
    walk (p:ps) = do
        case Text.uncons p of
            Just (c, rest)
                | '1' <= c && c <= '9' -> arg (fromEnum c - fromEnum '1'):rest:walk ps
                | otherwise -> rest:walk ps
            Nothing -> walk ps
    walk [] = []
    arg n = do
        case drop n arglist of
            x:_ -> Text.pack x
            _ -> ""
