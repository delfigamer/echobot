{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel.Vk.Internal
    ( Config(..)
    , withVkChannel
    , apiVersion
    ) where


import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Text
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as Vector
import qualified Channel
import qualified Logger
import qualified WebDriver


data Config
    = Config
        { cToken :: Text.Text
        , cGroupId :: Text.Text
        , cTimeout :: Int
        , cKeyboardWidth :: Int }
    deriving (Show)


withVkChannel :: Config -> Logger.Handle -> WebDriver.Handle -> (Channel.Handle -> IO r) -> IO r
withVkChannel conf logger driver body = do
    vkc <- vkcNew conf logger driver
    body $ Channel.Handle
        { Channel.poll = vkcPoll vkc
        , Channel.sendMessage = vkcSendMessage vkc
        , Channel.sendMedia = vkcSendMedia vkc
        , Channel.possessMedia = vkcPossessMedia vkc
        , Channel.updateMessage = vkcUpdateMessage vkc
        , Channel.answerQuery = vkcAnswerQuery vkc }


data VkChannel
    = VkChannel
        { vkcConfig :: Config
        , vkcLogger :: Logger.Handle
        , vkcDriver :: WebDriver.Handle
        , vkcPollServer :: IORef (Maybe VkPollServer) }


data VkPollServer
    = VkPollServer !WebDriver.Address !Text.Text !Text.Text


apiVersion :: Text.Text
apiVersion = "5.122"


vkcNew :: Config -> Logger.Handle -> WebDriver.Handle -> IO VkChannel
vkcNew conf logger driver = do
    ppollserver <- newIORef $! Nothing
    return $ VkChannel
        { vkcConfig = conf
        , vkcLogger = logger
        , vkcDriver = driver
        , vkcPollServer = ppollserver }


vkcPoll :: VkChannel -> IO [Channel.Event]
vkcPoll vkc = do
    mpollserver <- readIORef (vkcPollServer vkc)
    updates <- case mpollserver of
        Nothing -> do
            pollserver <- requestServer
            mupdates <- requestPoll pollserver
            case mupdates of
                Nothing -> failLongPoll
                Just updates -> return $ updates
        Just pollserver -> do
            mupdates <- requestPoll pollserver
            case mupdates of
                Nothing -> do
                    pollserver2 <- requestServer
                    mupdates2 <- requestPoll pollserver2
                    case mupdates2 of
                        Nothing -> failLongPoll
                        Just updates2 -> return $ updates2
                Just updates -> return $ updates
    eventParts <- forM updates $ \update -> do
        case fromJSON update of
            Error err -> do
                Logger.warn (vkcLogger vkc) $
                    "VkChannel: Failed to parse an update: " <> Text.pack err
                return $ []
            Success (VkUpdateUnknown typename) -> do
                Logger.info (vkcLogger vkc) $
                    "VkChannel: Unknown update type: " <> typename
                return $ []
            Success (VkUpdateEvent event) -> do
                return $ [event]
    return $ concat $ eventParts
    where
    requestServer :: IO VkPollServer
    requestServer = do
        Logger.info (vkcLogger vkc) $
            "VkChannel: Request a long poll server"
        resp <- WebDriver.request (vkcDriver vkc)
            (WebDriver.HttpsAddress "api.vk.com" ["method", "groups.getLongPollServer"])
            (object
                [ "v" .= apiVersion
                , "access_token" .= cToken (vkcConfig vkc)
                , "group_id" .= cGroupId (vkcConfig vkc)
                ])
        case resp of
            VkError err -> do
                Logger.err (vkcLogger vkc) $
                    "VkChannel: Failed to get a long poll server: " <> err
                fail "failed to get a long poll server"
            VkResponse pollserver -> do
                Logger.info (vkcLogger vkc) $
                    "VkChannel: Got a long poll server"
                writeIORef (vkcPollServer vkc) $! Just $! pollserver
                return $ pollserver
    requestPoll :: VkPollServer -> IO (Maybe [Value])
    requestPoll (VkPollServer pollAddress pollKey pollTs) = do
        resp <- WebDriver.request (vkcDriver vkc)
            pollAddress
            (object
                [ "key" .= pollKey
                , "ts" .= pollTs
                , "act" .= String "a_check"
                , "wait" .= cTimeout (vkcConfig vkc)
                ])
        case resp of
            VkPollFail -> do
                return $ Nothing
            VkPollUpdates newTs updates -> do
                writeIORef (vkcPollServer vkc) $! Just $! VkPollServer pollAddress pollKey newTs
                return $ Just $ updates
    failLongPoll :: IO a
    failLongPoll = do
        Logger.err (vkcLogger vkc) $
            "VkChannel: Failed to establish a persistent connection with a long poll server"
        fail "failed to connect with a long poll server"


vkcSendMessage
    :: VkChannel
    -> Channel.ChatId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text.Text ())
vkcSendMessage vkc chatId content buttons = do
    undefined


vkcSendSticker
    :: VkChannel
    -> Channel.ChatId
    -> Channel.FileId
    -> IO (Either Text.Text ())
vkcSendSticker vkc chatId sticker = do
    undefined


vkcSendMedia
    :: VkChannel
    -> Channel.ChatId
    -> Text.Text
    -> [Channel.SendableMedia]
    -> IO (Either Text.Text ())
vkcSendMedia vkc chatId caption group = do
    undefined


vkcPossessMedia
    :: VkChannel
    -> Channel.ForeignMedia
    -> IO (Either Text.Text Channel.SendableMedia)
vkcPossessMedia vkc media = do
    undefined


vkcUpdateMessage
    :: VkChannel
    -> Channel.ChatId
    -> Channel.MessageId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text.Text ())
vkcUpdateMessage vkc chatId messageId content buttons = do
    undefined


vkcAnswerQuery
    :: VkChannel
    -> Channel.QueryId
    -> Text.Text
    -> IO (Either Text.Text ())
vkcAnswerQuery vkc queryId text = do
    undefined


data VkResponse a
    = VkError !Text.Text
    | VkResponse a


instance FromJSON a => FromJSON (VkResponse a) where
    parseJSON = withObject "VkResponse" $ \v -> do
        msum
            [ VkResponse <$> v .: "response"
            , VkError <$> (v .: "error" >>= (.: "error_msg"))
            ]


instance FromJSON VkPollServer where
    parseJSON = withObject "VkPollServer" $ \v -> do
        url <- v .: "server"
        address <- parseUrl url
        key <- v .: "key"
        ts <- v .: "ts"
        return $ VkPollServer address key ts
        where
        parseUrl url = do
            Just urlbody <- return $ Text.stripPrefix "https://" url
            server:parts <- return $ Text.splitOn "/" urlbody
            return $ WebDriver.HttpsAddress server parts


data VkPollResponse
    = VkPollFail
    | VkPollUpdates !Text.Text [Value]


instance FromJSON VkPollResponse where
    parseJSON = withObject "VkPollResponse" $ \v -> do
        msum
            [ VkPollUpdates <$> v .: "ts" <*> v .:? "updates" .!= []
            , return $ VkPollFail
            ]


data VkUpdate
    = VkUpdateUnknown !Text.Text
    | VkUpdateEvent !Channel.Event


instance FromJSON VkUpdate where
    parseJSON = withObject "VkUpdate" $ \v -> do
        typename <- v .: "type"
        case typename of
            "message_new" -> v .: "object" >>= (.: "message") >>= parseMessage
            _ -> return $ VkUpdateUnknown $! typename
        where
        parseMessage = withObject "VkUpdateMessage" $ \v -> do
            text <- v .:? "text" .!= ""
            chatId <- v .: "peer_id"
            messageId <- v .: "conversation_message_id"
            attachments <- v .:? "attachments" .!= []
            case attachments of
                [] -> do
                    return $ VkUpdateEvent $! Channel.EventMessage
                        { Channel.eChatId = chatId
                        , Channel.eMessageId = messageId
                        , Channel.eMessage = Channel.plainText text
                        }
                _ -> do
                    return $ VkUpdateEvent $! Channel.EventMedia
                        { Channel.eChatId = chatId
                        , Channel.eCaption = text
                        , Channel.eMedia = if chatId >= 2000000000
                            then map getPossessableForeignMedia attachments
                            else map getResendableForeignMedia attachments
                        }
        getResendableForeignMedia (VkForeignMedia media) = media
        getPossessableForeignMedia (VkForeignMedia media@(Channel.ForeignMedia mediaType mediaId mediaUrl)) = do
            if Text.count "_" mediaId == 2
                then Channel.ForeignMedia mediaType ("!" <> mediaId) mediaUrl
                else media
{-
A non-obvious feature: when a message comes from a group chat (peer_id >= 2000000000), the access_key of its attachments is unusable.
That is, if you try to re-send the media with the corresponding set of (owner_id, id, access_key), it won't work.
We mark such attachments with a prefix "!" in front of their mediaId.
Attachments that come from personal chats (0 <= peer_id < 1000000000) work fine.
Attachments from group chats without an access_key - that is, public attachments - also work.
So, the problem affects specifically attachments that both come from a group chat, and have a non-empty access_key.
The only way to re-send such media is to download it through from the server, upload the file again and then send it as your own media.
Not all types of media can be directly downloaded, however (videos), so sometimes the download url will be missing as well.
This can produce attachments that are technically known to us, but are completely unusable: their mediaId wouldn't work, and there is no download url.
-}


newtype VkForeignMedia
    = VkForeignMedia Channel.ForeignMedia


instance FromJSON VkForeignMedia where
    parseJSON = withObject "VkForeignMedia" $ \v -> do
        typename <- v .: "type"
        case typename :: Text.Text of
            "photo" -> v .: "photo" >>= parsePhoto
            "video" -> v .: "video" >>= parseVideo
            "audio" -> v .: "audio" >>= parseAudio
            "doc" -> v .: "doc" >>= parseDoc
            "sticker" -> v .: "sticker" >>= parseSticker
            "audio_message" -> v .: "audio_message" >>= parseVoice
            _ -> return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaUnknown typename ""
        where
        parsePhoto = withObject "VkForeignMediaPhoto" $ \v -> do
            mediaId <- getMediaId "photo" v
            sizes <- v .:? "sizes" .!= []
            if null (sizes :: [VkPhotoSize])
                then return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaPhoto mediaId ""
                else do
                    let VkPhotoSize _ _ url = flip maximumBy sizes
                            $ \(VkPhotoSize w1 h1 _) (VkPhotoSize w2 h2 _) -> do
                                compare w1 w2 <> compare h1 h2
                    return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaPhoto mediaId url
        parseVideo = withObject "VkForeignMediaPhoto" $ \v -> do
            mediaId <- getMediaId "video" v
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaVideo mediaId ""
        parseAudio = withObject "VkForeignMediaAudio" $ \v -> do
            mediaId <- getMediaId "audio" v
            url <- v .:? "url" .!= ""
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaAudio mediaId url
        parseDoc = withObject "VkForeignMediaDocument" $ \v -> do
            mediaId <- getMediaId "doc" v
            url <- v .:? "url" .!= ""
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaDocument mediaId url
        parseSticker = withObject "VkForeignMediaSticker" $ \v -> do
            stickerId <- v .: "sticker_id"
            let stickerIdStr = show (stickerId :: Integer)
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaSticker (Text.pack stickerIdStr) ""
        parseVoice = withObject "VkForeignMediaVoice" $ \v -> do
            url <- v .: "link_ogg"
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaVoice "" url
        getMediaId typename v = do
            ownerId <- v .: "owner_id"
            let ownerIdStr = show (ownerId :: Integer)
            localId <- v .: "id"
            let localIdStr = show (localId :: Integer)
            maccessKey <- v .:? "access_key"
            case maccessKey of
                Nothing -> return $ Text.pack $ typename <> ownerIdStr <> "_" <> localIdStr
                Just accessKey -> return $ Text.pack $ typename <> ownerIdStr <> "_" <> localIdStr <> "_" <> accessKey


data VkPhotoSize
    = VkPhotoSize !Int !Int !Text.Text


instance FromJSON VkPhotoSize where
    parseJSON = withObject "VkPhotoSize" $ \v -> do
        VkPhotoSize
            <$> v .: "width"
            <*> v .: "height"
            <*> v .: "url"
