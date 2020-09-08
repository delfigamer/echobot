{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel.Tg
    ( Config(..)
    , withTgChannel
    ) where


import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Text
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Vector as Vector
import qualified Channel
import qualified Logger
import qualified WebDriver


data Config
    = Config
        { cToken :: Text
        , cTimeout :: Int
        , cKeyboardWidth :: Int }
    deriving (Show)


withTgChannel :: Config -> Logger.Handle -> WebDriver.Handle -> (Channel.Handle -> IO r) -> IO r
withTgChannel conf logger driver body = do
    tgc <- tgcNew conf logger driver
    body $ Channel.Handle
        { Channel.poll = tgcPoll tgc
        , Channel.sendMessage = tgcSendMessage tgc
        , Channel.sendSticker = tgcSendSticker tgc
        , Channel.sendMedia = tgcSendMedia tgc
        , Channel.sendMediaGroup = tgcSendMediaGroup tgc
        , Channel.updateMessage = tgcUpdateMessage tgc
        , Channel.answerQuery = tgcAnswerQuery tgc }


data TgChannel
    = TgChannel
        { tgcToken :: Text
        , tgcTimeout :: Int
        , tgcKeyboardWidth :: Int
        , tgcLogger :: Logger.Handle
        , tgcDriver :: WebDriver.Handle
        , tgcOffset :: IORef Integer }


tgcNew :: Config -> Logger.Handle -> WebDriver.Handle -> IO TgChannel
tgcNew conf logger driver = do
    poffset <- newIORef $! -1
    return $ TgChannel
        { tgcToken = cToken conf
        , tgcTimeout = cTimeout conf
        , tgcKeyboardWidth = cKeyboardWidth conf
        , tgcLogger = logger
        , tgcDriver = driver
        , tgcOffset = poffset }


tgcPoll :: TgChannel -> IO [Channel.Event]
tgcPoll tgc = do
    oldoffset <- readIORef (tgcOffset tgc)
    Logger.debug (tgcLogger tgc) $
        "TgChannel: Current update offset: " <> Text.pack (show oldoffset)
    Logger.info (tgcLogger tgc) $
        "TgChannel: Poll..."
    let offsetopt = if oldoffset < 0
        then []
        else ["offset" .= oldoffset]
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "getUpdates"])
        (object $ offsetopt <> ["timeout" .= tgcTimeout tgc])
    case resp of
        TgResponseOk tgupdates -> do
            Logger.info (tgcLogger tgc) $
                "TgChannel: Response received"
            let newoffset = maximum $
                    oldoffset
                    :map (\tgu -> tguOffset tgu + 1) tgupdates
            writeIORef (tgcOffset tgc) $! newoffset
            Logger.debug (tgcLogger tgc) $
                "TgChannel: New update offset: " <> Text.pack (show newoffset)
            events <- forM tgupdates $ \tgu -> do
                case tgu of
                    TgMessage _ msgv -> do
                        case fromJSON msgv of
                            Success (TgEventMessage ev) -> do
                                return $ [ev]
                            Error e -> do
                                Logger.warn (tgcLogger tgc) $
                                    "TgChannel: " <> Text.pack e
                                Logger.warn (tgcLogger tgc) $
                                    "TgChannel: Unknown message type:"
                                case msgv of
                                    Object v -> do
                                        Logger.warn (tgcLogger tgc) $
                                            "TgChannel: \t(w/fields) " <> Text.pack (show (HashMap.keys v))
                                    _ -> do
                                        Logger.warn (tgcLogger tgc) $
                                            "TgChannel: \t" <> Text.pack (show msgv)
                                return $ []
                    TgQuery _ queryv -> do
                        case fromJSON queryv of
                            Success (TgEventQuery ev) -> do
                                return $ [ev]
                            Error e -> do
                                Logger.warn (tgcLogger tgc) $
                                    "TgChannel: " <> Text.pack e
                                Logger.warn (tgcLogger tgc) $
                                    "TgChannel: Unknown callback query type:"
                                case queryv of
                                    Object v -> do
                                        Logger.warn (tgcLogger tgc) $
                                            "TgChannel: \t(w/fields) " <> Text.pack (show (HashMap.keys v))
                                    _ -> do
                                        Logger.warn (tgcLogger tgc) $
                                            "TgChannel: \t" <> Text.pack (show queryv)
                                return $ []
                    TgUnknown _ fields -> do
                        Logger.info (tgcLogger tgc) $
                            "TgChannel: Unknown update type:"
                        Logger.info (tgcLogger tgc) $
                            "TgChannel: \t(w/fields) " <> Text.pack (show fields)
                        return $ []
            return $ join $ events
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Request failed: " <> e
            if oldoffset >= 0
                then do
                    Logger.info (tgcLogger tgc) $
                        "TgChannel: Maybe because of invalid offset? Clear it and try again"
                    writeIORef (tgcOffset tgc) $! -1
                    return $ []
                else do
                    Logger.info (tgcLogger tgc) $
                        "TgChannel: Nothing we can do, shut down the channel"
                    fail "Telegram API error"


tgcSendMessage
    :: TgChannel
    -> Channel.ChatId
    -> Text
    -> [Channel.QueryButton]
    -> IO (Either Text Channel.MessageId)
tgcSendMessage tgc chatId text buttons = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Send message"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show text)
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show buttons)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "sendMessage"])
        (object $
            [ "chat_id" .= chatId
            , "text" .= text
            ] <> keyboardMarkupOpt (tgcKeyboardWidth tgc) buttons)
    case resp of
        TgResponseOk (TgSentMessageId messageId) -> do
            Logger.debug (tgcLogger tgc) $
                "TgChannel: Message sent: " <> Text.pack (show messageId)
            return $ Right messageId
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Message sending failed: " <> e
            return $ Left e


tgcSendSticker
    :: TgChannel
    -> Channel.ChatId
    -> Channel.FileId
    -> IO (Either Text ())
tgcSendSticker tgc chatId sticker = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Send sticker"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show sticker)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "sendSticker"])
        (object
            [ "chat_id" .= chatId
            , "sticker" .= sticker
            ])
    case resp of
        TgResponseOk (TgVoid _) -> do
            Logger.debug (tgcLogger tgc) $
                "TgChannel: Sticker sent"
            return $ Right ()
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Sticker sending failed: " <> e
            return $ Left e


tgcSendMedia
    :: TgChannel
    -> Channel.ChatId
    -> Text
    -> Channel.Media
    -> IO (Either Text ())
tgcSendMedia = undefined


tgcSendMediaGroup
    :: TgChannel
    -> Channel.ChatId
    -> Channel.MediaGroup
    -> IO (Either Text ())
tgcSendMediaGroup = undefined


tgcUpdateMessage
    :: TgChannel
    -> Channel.ChatId
    -> Channel.MessageId
    -> Text
    -> [Channel.QueryButton]
    -> IO (Either Text ())
tgcUpdateMessage tgc chatId messageId text buttons = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Update message"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show chatId) <> " <- "  <> Text.pack (show messageId) <> " <- " <> Text.pack (show text)
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show buttons)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "editMessageText"])
        (object $
            [ "chat_id" .= chatId
            , "message_id" .= messageId
            , "text" .= text
            ] <> keyboardMarkupOpt (tgcKeyboardWidth tgc) buttons)
    case resp of
        TgResponseOk (TgVoid _) -> do
            Logger.debug (tgcLogger tgc) $
                "TgChannel: Message updated"
            return $ Right ()
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Message update failed: " <> e
            return $ Left e


tgcAnswerQuery
    :: TgChannel
    -> Channel.QueryId
    -> Text
    -> IO (Either Text ())
tgcAnswerQuery tgc queryId text = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Answer query"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show queryId) <> " <- " <> Text.pack (show text)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "answerCallbackQuery"])
        (if Text.null text
            then object ["callback_query_id" .= queryId]
            else object ["callback_query_id" .= queryId, "text" .= text])
    case resp of
        TgResponseOk (TgVoid _) -> do
            Logger.debug (tgcLogger tgc) $
                "TgChannel: Query answered"
            return $ Right ()
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Query answer failed: " <> e
            return $ Left e


keyboardMarkupOpt :: KeyValue kv => Int -> [Channel.QueryButton] -> [kv]
keyboardMarkupOpt _ [] = []
keyboardMarkupOpt kbwidth buttons = do
    let buttonvalues = map
            (\(Channel.QueryButton title userdata) -> do
                object
                    [ "text" .= title
                    , "callback_data" .= userdata ])
            buttons
    let value = object ["inline_keyboard" .= sectionList kbwidth buttonvalues]
    ["reply_markup" .= TextLazy.toStrict (encodeToLazyText value)]
    where
    sectionList len xs = do
        case splitAt len xs of
            (_, []) -> [xs]
            (h, r) -> h:sectionList len r


data TgResponse a
    = TgResponseOk a
    | TgResponseErr Text


data TgUpdate
    = TgMessage
        { tguOffset :: !Integer
        , tguMessage :: !Value }
    | TgQuery
        { tguOffset :: !Integer
        , tguQuery :: !Value }
    | TgUnknown
        { tguOffset :: !Integer
        , tguSourceFields :: [Text] }


instance FromJSON a => FromJSON (TgResponse a) where
    parseJSON = withObject "TgResponse" $ \v -> do
        isOk <- v .: "ok"
        if isOk
            then TgResponseOk <$> v .: "result"
            else TgResponseErr <$> v .: "description"


instance FromJSON TgUpdate where
    parseJSON = withObject "TgEvent" $ \v -> do
        updateId <- v .: "update_id"
        msum
            [ TgMessage updateId <$> v .: "message"
            , TgQuery updateId <$> v .: "callback_query"
            , return $ TgUnknown updateId (HashMap.keys v) ]


newtype TgEventMessage = TgEventMessage Channel.Event


instance FromJSON TgEventMessage where
    parseJSON = withObject "TgMessage" $ \m -> do
        chatId <- m .: "chat" >>= (.: "id")
        msum
            [ parseMsgSticker chatId m
            , parseMsgMedia chatId m
            , parseMsgText chatId m
            ]
        where
        parseMsgSticker chatId m = do
            sticker <- m .: "sticker" >>= (.: "file_id")
            return $ TgEventMessage $ Channel.EventSticker
                { Channel.eChatId = chatId
                , Channel.eSticker = sticker }
        parseMsgMedia chatId m = do
            caption <- m .:? "caption" .!= ""
            media <- msum
                [ (m .: "photo" >>=) $ withArray "PhotoSizes" $ \sizes -> do
                    Just size0 <- return $ sizes Vector.!? 0
                    flip (withObject "Photo") size0 $ \photo -> do
                        Channel.MediaPhoto <$> photo .: "file_id"
                , Channel.MediaVideo <$> (m .: "video" >>= (.: "file_id"))
                , Channel.MediaAudio <$> (m .: "audio" >>= (.: "file_id"))
                , Channel.MediaAnimation <$> (m .: "animation" >>= (.: "file_id"))
                , Channel.MediaVoice <$> (m .: "voice" >>= (.: "file_id"))
                , Channel.MediaDocument <$> (m .: "document" >>= (.: "file_id"))
                ]
            return $ TgEventMessage $ Channel.EventMedia
                { Channel.eChatId = chatId
                , Channel.eCaption = caption
                , Channel.eMedia = media }
        parseMsgText chatId m = do
            messageId <- m .: "message_id"
            text <- m .: "text"
            return $ TgEventMessage $ Channel.EventMessage
                { Channel.eChatId = chatId
                , Channel.eMessageId = messageId
                , Channel.eMessage = text }


newtype TgEventQuery = TgEventQuery Channel.Event


instance FromJSON TgEventQuery where
    parseJSON = withObject "TgCallbackQuery" $ \e -> do
        queryId <- e .: "id"
        userdata <- e .: "data"
        (e .: "message" >>=) $ withObject "TgMessage" $ \m -> do
            chatId <- m .: "chat" >>= (.: "id")
            messageId <- m .: "message_id"
            return $ TgEventQuery $ Channel.EventQuery
                { Channel.eChatId = chatId
                , Channel.eMessageId = messageId
                , Channel.eQueryId = queryId
                , Channel.eUserdata = userdata }


newtype TgSentMessageId = TgSentMessageId Channel.MessageId


instance FromJSON TgSentMessageId where
    parseJSON = withObject "TgSentMessage" $ \v -> do
        TgSentMessageId <$> v .: "message_id"


newtype TgVoid = TgVoid ()


instance FromJSON TgVoid where
    parseJSON _ = return $ TgVoid ()
