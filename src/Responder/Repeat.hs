{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Responder.Repeat
    ( Config(..)
    , withRepeatResponder
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
import qualified Responder


data Config
    = Config
        { cDefaultMultiplier :: Int
        , cUnknownCommandMsg :: Text.Text
        , cDescribeCmd :: Text.Text
        , cDescribeMsg :: Text.Text
        , cInspectMultiplierCmd :: Text.Text
        , cInspectMultiplierMsg :: Text.Text
        , cMultiplierSetMsg :: Text.Text
        , cMaxMultiplier :: Int }
    deriving (Show)


withRepeatResponder :: Config -> Logger.Handle -> Channel.Handle -> (Responder.Handle -> IO r) -> IO r
withRepeatResponder conf logger channel body = do
    er <- erNew conf logger channel
    body $ Responder.Handle
        { Responder.work = erWork er }


data RepeatResponder
    = RepeatResponder
        { erConfig :: Config
        , erLogger :: Logger.Handle
        , erChannel :: Channel.Handle
        , erContextTable :: IOTable.CuckooHashTable Channel.ChatId ChatContext }


data ChatContext
    = ChatContext
        { ccMultiplier :: IORef (Maybe Int) }


erNew :: Config -> Logger.Handle -> Channel.Handle -> IO RepeatResponder
erNew conf logger channel = do
    ctab <- IOTable.new
    return $ RepeatResponder
        { erConfig = conf
        , erLogger = logger
        , erChannel = channel
        , erContextTable = ctab }


erWork :: RepeatResponder -> IO ()
erWork er = do
    events <- Channel.poll (erChannel er)
    mapM_ (erHandleEvent er) events


erHandleEvent :: RepeatResponder -> Channel.Event -> IO ()
erHandleEvent er (Channel.EventMessage chatId _ text) = do
    if Text.isPrefixOf "/" text
        then erHandleCommand er chatId text
        else erRepeatMessage er chatId text
erHandleEvent er (Channel.EventSticker chatId sticker) = do
    erRepeatSticker er chatId sticker
erHandleEvent er (Channel.EventQuery chatId messageId queryId userdata) = do
    erMakeRequest er "answerQuery" $
        Channel.answerQuery (erChannel er) queryId ""
    case Text.stripPrefix "r" userdata of
        Just mt
            | (mult, _):_ <- reads (Text.unpack mt)
            , 1 <= mult && mult <= cMaxMultiplier (erConfig er) -> do
                erSetMultiplier er chatId mult
                erMakeRequest er "updateMessage" $
                    Channel.updateMessage (erChannel er) chatId messageId
                        (substitute [show mult] $ cMultiplierSetMsg (erConfig er))
                        []
        _ -> return ()


erRepeatMessage :: RepeatResponder -> Channel.ChatId -> Text.Text -> IO ()
erRepeatMessage er chatId text = do
    mult <- erGetMultiplier er chatId
    Logger.debug (erLogger er) $
        "Responder: repeat a message into " <> Text.pack (show chatId) <> " " <> Text.pack (show mult) <> " times"
    replicateM_ mult $ do
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId text []


erRepeatSticker :: RepeatResponder -> Channel.ChatId -> Channel.StickerName -> IO ()
erRepeatSticker er chatId sticker = do
    mult <- erGetMultiplier er chatId
    Logger.debug (erLogger er) $
        "Responder: repeat a sticker into " <> Text.pack (show chatId) <> " " <> Text.pack (show mult) <> " times"
    replicateM_ mult $ do
        erMakeRequest er "sendSticker" $
            Channel.sendSticker (erChannel er) chatId sticker


erHandleCommand :: RepeatResponder -> Channel.ChatId -> Text.Text -> IO ()
erHandleCommand er chatId cmd
    | Text.isPrefixOf (cDescribeCmd (erConfig er)) cmd = do
        mult <- erGetMultiplier er chatId
        Logger.debug (erLogger er) $
            "Responder: requested description in " <> Text.pack (show chatId)
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (substitute [show mult] $ cDescribeMsg (erConfig er))
                []
    | Text.isPrefixOf (cInspectMultiplierCmd (erConfig er)) cmd = do
        mult <- erGetMultiplier er chatId
        Logger.debug (erLogger er) $
            "Responder: requested multiplier in " <> Text.pack (show chatId) <> " (currently " <> Text.pack (show mult) <> ")"
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (substitute [show mult] $ cInspectMultiplierMsg (erConfig er))
                multiplierButtons
    | otherwise = do
        Logger.debug (erLogger er) $
            "Responder: unknown command in " <> Text.pack (show chatId)
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (cUnknownCommandMsg (erConfig er))
                []
    where
    multiplierButtons = do
        map
            (\i -> Channel.QueryButton (Text.pack (show i)) (Text.pack ("r" ++ show i)))
            [1 .. cMaxMultiplier (erConfig er)]


erGetMultiplier :: RepeatResponder -> Channel.ChatId -> IO Int
erGetMultiplier er chatId = do
    mcc <- IOTable.lookup (erContextTable er) chatId
    case mcc of
        Nothing -> return $ cDefaultMultiplier (erConfig er)
        Just cc -> do
            mmult <- readIORef (ccMultiplier cc)
            case mmult of
                Nothing -> return $ cDefaultMultiplier (erConfig er)
                Just mult -> return mult


erSetMultiplier :: RepeatResponder -> Channel.ChatId -> Int -> IO ()
erSetMultiplier er chatId mult = do
    cc <- erProvideChatContext er chatId
    writeIORef (ccMultiplier cc) $! Just mult
    Logger.info (erLogger er) $
        "Responder: set multiplier in " <> Text.pack (show chatId) <> " to " <> Text.pack (show mult)


erProvideChatContext :: RepeatResponder -> Channel.ChatId -> IO ChatContext
erProvideChatContext er chatId = do
    mcc <- IOTable.lookup (erContextTable er) chatId
    case mcc of
        Nothing -> do
            cc <- ChatContext
                <$> newIORef Nothing
            IOTable.insert (erContextTable er) chatId cc
            Logger.info (erLogger er) $
                "Responder: allocate a new context in " <> Text.pack (show chatId)
            return $ cc
        Just cc -> do
            return $ cc


erMakeRequest :: RepeatResponder -> Text.Text -> IO (Either Text.Text a) -> IO ()
erMakeRequest er reqname act = do
    r <- act
    case r of
        Right _ -> return ()
        Left err -> do
            Logger.warn (erLogger er) $
                "Responder: " <> reqname <> " failed: " <> err


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
