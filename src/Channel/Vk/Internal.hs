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
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Text
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as Vector
import qualified System.Random as Random
import qualified Text.URI as URI
import qualified Channel
import qualified Logger
import qualified WebDriver


data Config
    = Config
        { cToken :: Text.Text
        , cGroupId :: Integer
        , cTimeout :: Integer
        , cKeyboardWidth :: Int }
    deriving (Show)


withVkChannel :: Config -> Int -> Logger.Handle -> WebDriver.Handle -> (Channel.Handle -> IO r) -> IO r
withVkChannel conf seed logger driver body = do
    vkc <- vkcNew conf seed logger driver
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
        , vkcPollServer :: IORef (Maybe VkPollServer)
        , vkcRandomState :: IORef Random.StdGen }


data VkPollServer
    = VkPollServer !WebDriver.Address !Text.Text !Text.Text


apiVersion :: Text.Text
apiVersion = "5.122"


vkcNew :: Config -> Int -> Logger.Handle -> WebDriver.Handle -> IO VkChannel
vkcNew conf seed logger driver = do
    ppollserver <- newIORef $! Nothing
    prandomstate <- newIORef $! Random.mkStdGen seed
    return $ VkChannel
        { vkcConfig = conf
        , vkcLogger = logger
        , vkcDriver = driver
        , vkcPollServer = ppollserver
        , vkcRandomState = prandomstate }


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
            "https://api.vk.com/method/groups.getLongPollServer"
            [ WebDriver.ParamText "v" $ apiVersion
            , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
            , WebDriver.ParamNum "group_id" $ cGroupId $ vkcConfig vkc
            ]
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
        Logger.debug (vkcLogger vkc) $
            "VkChannel: Current update offset: " <> Text.pack (show pollTs)
        Logger.info (vkcLogger vkc) $
            "VkChannel: Poll..."
        resp <- WebDriver.request (vkcDriver vkc)
            pollAddress
            [ WebDriver.ParamText "key" $ pollKey
            , WebDriver.ParamText "ts" $ pollTs
            , WebDriver.ParamText "act" $ "a_check"
            , WebDriver.ParamNum "wait" $ cTimeout $ vkcConfig vkc
            ]
        case resp of
            VkPollFail -> do
                Logger.err (vkcLogger vkc) $
                    "VkChannel: Request failed"
                return $ Nothing
            VkPollUpdates newTs updates -> do
                Logger.info (vkcLogger vkc) $
                    "VkChannel: Response received"
                Logger.debug (vkcLogger vkc) $
                    "VkChannel: New update offset: " <> Text.pack (show newTs)
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
    -> IO (Either Text.Text Channel.MessageId)
vkcSendMessage vkc chatId content buttons = do
    Logger.info (vkcLogger vkc) $
        "VkChannel: Send message"
    Logger.debug (vkcLogger vkc) $
        "VkChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show content)
    Logger.debug (vkcLogger vkc) $
        "VkChannel: \t" <> Text.pack (show buttons)
    let text = Builder.toLazyText $ encodeRichTextToBuilder content
    if TextLazy.null text
        then return $ Right 0
        else do
            randomState1 <- readIORef (vkcRandomState vkc)
            let (randomId, randomState2) = Random.random randomState1
            writeIORef (vkcRandomState vkc) $! randomState2
            resp <- WebDriver.request (vkcDriver vkc)
                "https://api.vk.com/method/messages.send"
                (   [ WebDriver.ParamText "v" $ apiVersion
                    , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
                    , WebDriver.ParamNum "peer_id" $ chatId
                    , WebDriver.ParamNum "random_id" $ toInteger (randomId :: Word)
                    , WebDriver.ParamTextLazy "message" $ text
                    ]
                <> keyboardMarkupOpt (cKeyboardWidth (vkcConfig vkc)) buttons)
            case resp of
                VkError e -> do
                    Logger.err (vkcLogger vkc) $
                        "VkChannel: Message sending failed: " <> e
                    return $ Left e
                VkResponse messageId -> do
                    Logger.info (vkcLogger vkc) $
                        "VkChannel: Message sent"
                    return $ Right messageId


vkcSendMedia
    :: VkChannel
    -> Channel.ChatId
    -> Text.Text
    -> [Channel.SendableMedia]
    -> IO (Either Text.Text ())
vkcSendMedia vkc chatId caption0 group = do
    Logger.info (vkcLogger vkc) $
        "VkChannel: Send media"
    Logger.debug (vkcLogger vkc) $
        "VkChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show caption0)
    Logger.debug (vkcLogger vkc) $
        "VkChannel: \t" <> Text.pack (show group)
    let (trivialLeft, rest) = break isNontrivialMedia group
    doTrivial (Right ()) caption0 trivialLeft rest
    where
    doTrivial current "" [] rest = doNext current rest
    doTrivial current caption subgroup rest = do
        Logger.debug (vkcLogger vkc) $
            "VkChannel: Send trivial: " <> Text.pack (show subgroup)
        randomState1 <- readIORef (vkcRandomState vkc)
        let (randomId, randomState2) = Random.random randomState1
        writeIORef (vkcRandomState vkc) $! randomState2
        resp <- WebDriver.request (vkcDriver vkc)
            "https://api.vk.com/method/messages.send"
            [ WebDriver.ParamText "v" $ apiVersion
            , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
            , WebDriver.ParamNum "peer_id" $ chatId
            , WebDriver.ParamNum "random_id" $ toInteger (randomId :: Word)
            , WebDriver.ParamText "message" $ caption
            , WebDriver.ParamText "attachment" $ Text.intercalate "," $ map (\(Channel.SendableMedia _ mediaId) -> mediaId) $ subgroup
            ]
        case resp of
            VkError e -> do
                Logger.err (vkcLogger vkc) $
                    "VkChannel: Media sending failed: " <> e
                doNext (current >> Left e) rest
            VkResponse (VkVoid _) -> do
                doNext current rest
    doNext current [] = return current
    doNext current (Channel.SendableMedia Channel.MediaSticker stickerId:rest) = do
        Logger.debug (vkcLogger vkc) $
            "VkChannel: Send sticker: " <> Text.pack (show stickerId)
        randomState1 <- readIORef (vkcRandomState vkc)
        let (randomId, randomState2) = Random.random randomState1
        writeIORef (vkcRandomState vkc) $! randomState2
        resp <- WebDriver.request (vkcDriver vkc)
            "https://api.vk.com/method/messages.send"
            [ WebDriver.ParamText "v" $ apiVersion
            , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
            , WebDriver.ParamNum "peer_id" $ chatId
            , WebDriver.ParamNum "random_id" $ toInteger (randomId :: Word)
            , WebDriver.ParamText "sticker_id" $ stickerId
            ]
        case resp of
            VkError e -> do
                Logger.err (vkcLogger vkc) $
                    "VkChannel: Sticker sending failed: " <> e
                doNext (current >> Left e) rest
            VkResponse (VkVoid _) -> do
                doNext current rest
    doNext current subgroup = do
        let (trivialLeft, rest) = break isNontrivialMedia subgroup
        doTrivial current "" trivialLeft rest
    isNontrivialMedia (Channel.SendableMedia Channel.MediaSticker _) = True
    isNontrivialMedia _ = False


vkcPossessMedia
    :: VkChannel
    -> Channel.ChatId
    -> Channel.ForeignMedia
    -> IO Channel.PossessMediaOutcome
vkcPossessMedia vkc chatId (Channel.ForeignMedia mediaType mediaId mediaUrl) = do
    case mediaType of
        Channel.MediaUnknown -> return $ Channel.PossessMediaUnknownType $! mediaId
        _
            | not (Text.null mediaId || Text.isPrefixOf "!" mediaId) -> return $ Channel.PossessMediaSuccess $! Channel.SendableMedia mediaType mediaId
            | Text.null mediaUrl -> return $ Channel.PossessMediaUnsupported
            | otherwise -> do
                r <- try $ vkcReuploadMedia vkc chatId mediaType mediaUrl
                case r of
                    Left e -> do
                        Logger.err (vkcLogger vkc) $
                            "VkChannel: Failed to possess media: " <> Text.pack (show (e :: SomeException))
                        return $ Channel.PossessMediaInternalError
                    Right result -> return $ Channel.PossessMediaSuccess $! result


vkcUpdateMessage
    :: VkChannel
    -> Channel.ChatId
    -> Channel.MessageId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text.Text ())
vkcUpdateMessage vkc chatId messageId content buttons = do
    Logger.info (vkcLogger vkc) $
        "VkChannel: Update message"
    Logger.debug (vkcLogger vkc) $
        "VkChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show messageId) <> " <- " <> Text.pack (show content)
    Logger.debug (vkcLogger vkc) $
        "VkChannel: \t" <> Text.pack (show buttons)
    resp <- if messageId <= 0
        then do
            randomState1 <- readIORef (vkcRandomState vkc)
            let (randomId, randomState2) = Random.random randomState1
            writeIORef (vkcRandomState vkc) $! randomState2
            WebDriver.request (vkcDriver vkc)
                "https://api.vk.com/method/messages.send"
                (   [ WebDriver.ParamText "v" $ apiVersion
                    , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
                    , WebDriver.ParamNum "peer_id" $ chatId
                    , WebDriver.ParamNum "random_id" $ toInteger (randomId :: Word)
                    , WebDriver.ParamTextLazy "message" $ Builder.toLazyText $ encodeRichTextToBuilder content
                    ]
                <> keyboardMarkupOpt (cKeyboardWidth (vkcConfig vkc)) buttons)
        else do
            WebDriver.request (vkcDriver vkc)
                "https://api.vk.com/method/messages.edit"
                (   [ WebDriver.ParamText "v" $ apiVersion
                    , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
                    , WebDriver.ParamNum "peer_id" $ chatId
                    , WebDriver.ParamNum "message_id" $ messageId
                    , WebDriver.ParamTextLazy "message" $ Builder.toLazyText $ encodeRichTextToBuilder content
                    ]
                <> keyboardMarkupOpt (cKeyboardWidth (vkcConfig vkc)) buttons)
    case resp of
        VkError e -> do
            Logger.err (vkcLogger vkc) $
                "VkChannel: Message updating failed: " <> e
            return $ Left e
        VkResponse (VkVoid _) -> do
            Logger.info (vkcLogger vkc) $
                "VkChannel: Message updated"
            return $ Right ()


vkcAnswerQuery
    :: VkChannel
    -> Channel.QueryId
    -> Text.Text
    -> IO (Either Text.Text ())
vkcAnswerQuery vkc queryId text = do
    return $ Right ()


vkcReuploadMedia
    :: VkChannel
    -> Channel.ChatId
    -> Channel.MediaType
    -> Text.Text
    -> IO Channel.SendableMedia
vkcReuploadMedia vkc chatId Channel.MediaPhoto mediaUrl = do
    vkcReuploadMediaGeneral vkc chatId
        mediaUrl
        (extractFilename mediaUrl)
        "https://api.vk.com/method/photos.getMessagesUploadServer"
        []
        "https://api.vk.com/method/photos.saveMessagesPhoto"
        (\(VkPhotoUploadResult server photo hash) ->
            [ WebDriver.ParamNum "server" $ server
            , WebDriver.ParamText "photo" $ photo
            , WebDriver.ParamText "hash" $ hash
            ])
        (\resultList -> do
            [VkSendablePhoto result] <- resultList
            Just result)
vkcReuploadMedia vkc chatId Channel.MediaDocument combinedUrl = do
    let (filename, mediaUrl) = fromMaybe ("", combinedUrl) $ do
            (fn, urlstr):_ <- Just $ reads $ Text.unpack combinedUrl
            Just $ (fn, Text.pack urlstr)
    vkcReuploadMediaGeneral vkc chatId
        mediaUrl
        filename
        "https://api.vk.com/method/docs.getMessagesUploadServer"
        [WebDriver.ParamText "type" $ "doc"]
        "https://api.vk.com/method/docs.save"
        (\(VkDocumentUploadResult file) -> [WebDriver.ParamText "file" $ file])
        (\(VkSendableDocument result) -> Just result)
vkcReuploadMedia vkc chatId Channel.MediaVoice mediaUrl = do
    vkcReuploadMediaGeneral vkc chatId
        mediaUrl
        (extractFilename mediaUrl)
        "https://api.vk.com/method/docs.getMessagesUploadServer"
        [WebDriver.ParamText "type" $ "audio_message"]
        "https://api.vk.com/method/docs.save"
        (\(VkDocumentUploadResult file) -> [WebDriver.ParamText "file" $ file])
        (\(VkSendableVoice result) -> Just result)
vkcReuploadMedia vkc _ _ _ = fail "unknown media type"


vkcReuploadMediaGeneral
    :: (FromJSON uploadResult, FromJSON saveResult)
    => VkChannel
    -> Channel.ChatId
    -> WebDriver.Address
    -> Text.Text
    -> WebDriver.Address
    -> [WebDriver.Param]
    -> WebDriver.Address
    -> (uploadResult -> [WebDriver.Param])
    -> (saveResult -> Maybe Channel.SendableMedia)
    -> IO Channel.SendableMedia
vkcReuploadMediaGeneral vkc chatId mediaUrl filename uploadMethodUrl uploadParams saveMethodUrl saveParamsFunc decodeMediaFunc = do
    Logger.info (vkcLogger vkc) $
        "VkChannel: Possess media " <> mediaUrl <> " : " <> filename
    Logger.debug (vkcLogger vkc) $
        "VkChannel: Request an upload server"
    resp <- WebDriver.request (vkcDriver vkc)
        uploadMethodUrl
        (   [ WebDriver.ParamText "v" $ apiVersion
            , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
            , WebDriver.ParamNum "peer_id" $ chatId
            ]
        <> uploadParams)
    uploadAddress <- case resp of
        VkError err -> do
            Logger.err (vkcLogger vkc) $
                "VkChannel: Failed to get an upload server: " <> err
            fail "failed to get an upload server"
        VkResponse (VkUploadAddress address) -> do
            Logger.debug (vkcLogger vkc) $
                "VkChannel: Got an upload server"
            return $ address
    Logger.debug (vkcLogger vkc) $
        "VkChannel: Download the media"
    mediaContent <- WebDriver.download (vkcDriver vkc) mediaUrl
    Logger.debug (vkcLogger vkc) $
        "VkChannel: Upload the media"
    uploadResult <- WebDriver.request (vkcDriver vkc)
        uploadAddress
        [ WebDriver.ParamFile "file" filename mediaContent
        ]
    Logger.debug (vkcLogger vkc) $
        "VkChannel: Save the media"
    resp <- WebDriver.request (vkcDriver vkc)
        saveMethodUrl
        (   [ WebDriver.ParamText "v" $ apiVersion
            , WebDriver.ParamText "access_token" $ cToken $ vkcConfig vkc
            ]
        <> saveParamsFunc uploadResult)
    case resp of
        VkError err -> do
            Logger.err (vkcLogger vkc) $
                "VkChannel: Failed to save uploaded media: " <> err
            fail "failed to save uploaded media"
        VkResponse saveResponse -> do
            case decodeMediaFunc saveResponse of
                Nothing -> do
                    Logger.err (vkcLogger vkc) $
                        "VkChannel: Failed to save uploaded media: unexpected response structure"
                    fail "unexpected response structure"
                Just result -> do
                    Logger.info (vkcLogger vkc) $
                        "VkChannel: Media saved as " <> Text.pack (show result)
                    return $ result


extractFilename :: Text.Text -> Text.Text
extractFilename address = do
    fromMaybe "" $ do
        uri <- URI.mkURI address
        (_, pathElems) <- URI.uriPath uri
        return $ URI.unRText $ NonEmpty.last $ pathElems


encodeRichTextToBuilder :: Channel.RichText -> Builder.Builder
encodeRichTextToBuilder (Channel.RichTextSpan _ text next) = do
    Builder.fromText text <> encodeRichTextToBuilder next
encodeRichTextToBuilder (Channel.RichTextLink href title next) = do
    encodeRichTextToBuilder title <> " (" <> Builder.fromText href <> ")" <> encodeRichTextToBuilder next
encodeRichTextToBuilder (Channel.RichTextMention user title next) = do
    encodeRichTextToBuilder title <> " (https://vk.com/id" <> Builder.fromText user <> ")" <> encodeRichTextToBuilder next
encodeRichTextToBuilder (Channel.RichTextMono text next) = do
    Builder.fromText text <> encodeRichTextToBuilder next
encodeRichTextToBuilder (Channel.RichTextCode _ text next) = do
    Builder.fromText text <> encodeRichTextToBuilder next
encodeRichTextToBuilder Channel.RichTextEnd = mempty


keyboardMarkupOpt :: Int -> [Channel.QueryButton] -> [WebDriver.Param]
keyboardMarkupOpt _ [] = []
keyboardMarkupOpt kbwidth buttons = do
    let buttonvalues = map
            (\(Channel.QueryButton title userdata) -> do
                object
                    [ "action" .= object
                        [ "type" .= String "text"
                        , "label" .= title
                        , "payload" .= show userdata
                        ]
                    ])
            buttons
    return $ WebDriver.ParamJson "keyboard" $
        object
            [ "buttons" .= sectionList kbwidth buttonvalues
            , "one_time" .= True
            ]
    where
    sectionList len xs = do
        case splitAt len xs of
            (_, []) -> [xs]
            (h, r) -> h:sectionList len r


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
        address <- v .: "server"
        key <- v .: "key"
        ts <- v .: "ts"
        return $ VkPollServer address key ts


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
            msum
                [ do
                    payload <- v .: "payload"
                    Just payloadInner <- return $ Text.stripSuffix "\"" =<< Text.stripPrefix "\"" payload
                    return $ VkUpdateEvent $! Channel.EventQuery
                        { Channel.eChatId = chatId
                        , Channel.eMessageId = 0
                        , Channel.eQueryId = ""
                        , Channel.eUserdata = Text.replace "\\\\" "\\" $ Text.replace "\\\"" "\"" $ payloadInner
                        }
                , do
                    attachments <- v .: "attachments"
                    guard $ not $ null attachments
                    return $ VkUpdateEvent $! Channel.EventMedia
                        { Channel.eChatId = chatId
                        , Channel.eCaption = text
                        , Channel.eMedia = if chatId >= 2000000000
                            then map getPossessableForeignMedia attachments
                            else map getResendableForeignMedia attachments
                        }
                , do
                    return $ VkUpdateEvent $! Channel.EventMessage
                        { Channel.eChatId = chatId
                        , Channel.eMessageId = messageId
                        , Channel.eMessage = Channel.plainText text
                        }
                ]
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
            mediaId <- getPublicMediaId "doc" v
            url <- v .:? "url" .!= ""
            title <- v .:? "title" .!= "a"
            mext <- v .:? "ext"
            let ext = maybe "" ('.':) mext
            let filename = if isSuffixOf ext title
                    then title
                    else title ++ ext
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaDocument mediaId (Text.pack (show filename) <> url)
        parseSticker = withObject "VkForeignMediaSticker" $ \v -> do
            stickerId <- v .: "sticker_id"
            let stickerIdStr = show (stickerId :: Integer)
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaSticker (Text.pack stickerIdStr) ""
        parseVoice = withObject "VkForeignMediaVoice" $ \v -> do
            url <- v .: "link_mp3"
            return $ VkForeignMedia $! Channel.ForeignMedia Channel.MediaVoice "" url


data VkPhotoSize
    = VkPhotoSize !Int !Int !Text.Text


instance FromJSON VkPhotoSize where
    parseJSON = withObject "VkPhotoSize" $ \v -> do
        VkPhotoSize
            <$> v .: "width"
            <*> v .: "height"
            <*> v .: "url"


newtype VkUploadAddress
    = VkUploadAddress Text.Text


instance FromJSON VkUploadAddress where
    parseJSON = withObject "VkUploadAddress" $ \v -> do
        VkUploadAddress <$> v .: "upload_url"


data VkPhotoUploadResult
    = VkPhotoUploadResult !Integer !Text.Text !Text.Text


instance FromJSON VkPhotoUploadResult where
    parseJSON = withObject "VkPhotoUploadResult" $ \v -> do
        VkPhotoUploadResult
            <$> v .: "server"
            <*> v .: "photo"
            <*> v .: "hash"


newtype VkSendablePhoto
    = VkSendablePhoto Channel.SendableMedia


instance FromJSON VkSendablePhoto where
    parseJSON = withObject "VkSendablePhoto" $ \v -> do
        mediaId <- getMediaId "photo" v
        return $ VkSendablePhoto $! Channel.SendableMedia Channel.MediaPhoto mediaId


newtype VkDocumentUploadResult
    = VkDocumentUploadResult Text.Text


instance FromJSON VkDocumentUploadResult where
    parseJSON = withObject "VkDocumentUploadResult" $ \v -> do
        VkDocumentUploadResult <$> v .: "file"


newtype VkSendableDocument
    = VkSendableDocument Channel.SendableMedia


instance FromJSON VkSendableDocument where
    parseJSON = withObject "VkSendableDocument" $ \v -> do
        v .: "doc" >>= parseDocument
        where
        parseDocument = withObject "VkMediaDocument" $ \v -> do
            mediaId <- getMediaId "doc" v
            return $ VkSendableDocument $! Channel.SendableMedia Channel.MediaDocument mediaId


newtype VkSendableVoice
    = VkSendableVoice Channel.SendableMedia


instance FromJSON VkSendableVoice where
    parseJSON = withObject "VkSendableVoice" $ \v -> do
        v .: "audio_message" >>= parseVoice
        where
        parseVoice = withObject "VkMediaVoice" $ \v -> do
            mediaId <- getMediaId "doc" v
            return $ VkSendableVoice $! Channel.SendableMedia Channel.MediaVoice mediaId


getPublicMediaId :: String -> Object -> Parser Text.Text
getPublicMediaId typename v = do
    ownerId <- v .: "owner_id"
    let ownerIdStr = show (ownerId :: Integer)
    localId <- v .: "id"
    let localIdStr = show (localId :: Integer)
    maccessKey <- v .:? "access_key"
    case maccessKey :: Maybe String of
        Nothing -> return $ Text.pack $ typename <> ownerIdStr <> "_" <> localIdStr
        Just accessKey -> return $ ""


getMediaId :: String -> Object -> Parser Text.Text
getMediaId typename v = do
    ownerId <- v .: "owner_id"
    let ownerIdStr = show (ownerId :: Integer)
    localId <- v .: "id"
    let localIdStr = show (localId :: Integer)
    maccessKey <- v .:? "access_key"
    case maccessKey of
        Nothing -> return $ Text.pack $ typename <> ownerIdStr <> "_" <> localIdStr
        Just accessKey -> return $ Text.pack $ typename <> ownerIdStr <> "_" <> localIdStr <> "_" <> accessKey


newtype VkVoid = VkVoid ()


instance FromJSON VkVoid where
    parseJSON _ = return $ VkVoid ()
