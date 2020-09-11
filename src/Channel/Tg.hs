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
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import Data.Text (Text)
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
            events <- parseUpdates tgupdates
            return $ groupEvents events
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
    where
    parseUpdates :: [TgUpdate] -> IO [Either Channel.Event TgGroupElem]
    parseUpdates [] = return $ []
    parseUpdates (tgu:rest) = do
        case tgu of
            TgMessage _ value -> do
                    parseOrWarnOne "message" value
                        (parseUpdates rest)
                        $ \(TgEventMessage mgroupId ev) -> do
                            (toGroupElem mgroupId ev:) <$> parseUpdates rest
            TgQuery _ value -> do
                parseOrWarnOne "callback query" value
                    (parseUpdates rest)
                    $ \(TgEventQuery ev) -> do
                        (Left ev:) <$> parseUpdates rest
            TgUnknown _ fields -> do
                Logger.info (tgcLogger tgc) $
                    "TgChannel: Unknown update type:"
                Logger.info (tgcLogger tgc) $
                    "TgChannel: \t(w/fields) " <> Text.pack (show fields)
                parseUpdates rest

    parseOrWarnOne :: FromJSON a => Text -> Value -> IO r -> (a -> IO r) -> IO r
    parseOrWarnOne what value onError onSuccess = do
        case fromJSON value of
            Success x -> do
                onSuccess x
            Error e -> do
                Logger.warn (tgcLogger tgc) $
                    "TgChannel: " <> Text.pack e
                Logger.warn (tgcLogger tgc) $
                    "TgChannel: Unknown " <> what <> " type:"
                case value of
                    Object v -> do
                        Logger.warn (tgcLogger tgc) $
                            "TgChannel: \t(w/fields) " <> Text.pack (show (HashMap.keys v))
                    Array _ -> do
                        Logger.warn (tgcLogger tgc) $
                            "TgChannel: \t(array)"
                    _ -> do
                        Logger.warn (tgcLogger tgc) $
                            "TgChannel: \t" <> Text.pack (show value)
                onError


groupEvents :: [Either Channel.Event TgGroupElem] -> [Channel.Event]
groupEvents [] = []
groupEvents (Left ev:rest) = ev:groupEvents rest
groupEvents (Right (TgGroupElem groupId chatId elem):rest) = takeGroup groupId chatId elem rest


takeGroup :: Channel.MediaGroupId -> Channel.ChatId -> (Channel.MediaGroup -> Channel.MediaGroup) -> [Either Channel.Event TgGroupElem] -> [Channel.Event]
takeGroup groupId chatId elem rest = do
    case rest of
        Right (TgGroupElem groupId2 chatId2 elem2):rest2
            | groupId == groupId2
            , chatId == chatId2 -> do
                takeGroup groupId chatId (elem . elem2) rest2
        _ -> do
            let mine = Channel.EventMediaGroup chatId groupId (elem Channel.MediaGroupEnd)
            mine:groupEvents rest


toGroupElem :: Maybe Channel.MediaGroupId -> Channel.Event -> Either Channel.Event TgGroupElem
toGroupElem (Just groupId) (Channel.EventMedia chatId caption (Channel.MediaPhoto fileId)) = Right $ TgGroupElem groupId chatId $ Channel.MediaGroupPhoto caption fileId
toGroupElem (Just groupId) (Channel.EventMedia chatId caption (Channel.MediaVideo fileId)) = Right $ TgGroupElem groupId chatId $ Channel.MediaGroupVideo caption fileId
toGroupElem _ ev = Left ev


tgcSendMessage
    :: TgChannel
    -> Channel.ChatId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text Channel.MessageId)
tgcSendMessage tgc chatId content buttons = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Send message"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show content)
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show buttons)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "sendMessage"])
        (object $
            [ "chat_id" .= chatId
            ] <> richTextOpt content <> keyboardMarkupOpt (tgcKeyboardWidth tgc) buttons)
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
tgcSendMedia tgc chatId caption media = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Send media"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show chatId) <> " <- "  <> Text.pack (show caption)
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show media)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, mediaRequestAddress media])
        (object $
            [ "chat_id" .= chatId
            ] <> mediaRequestData media <> mediaCaptionOpt caption)
    case resp of
        TgResponseOk (TgVoid _) -> do
            Logger.debug (tgcLogger tgc) $
                "TgChannel: Media sent"
            return $ Right ()
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Media sending failed: " <> e
            return $ Left e


mediaRequestAddress :: Channel.Media -> Text
mediaRequestAddress (Channel.MediaPhoto _) = "sendPhoto"
mediaRequestAddress (Channel.MediaVideo _) = "sendVideo"
mediaRequestAddress (Channel.MediaAudio _) = "sendAudio"
mediaRequestAddress (Channel.MediaAnimation _) = "sendAnimation"
mediaRequestAddress (Channel.MediaVoice _) = "sendVoice"
mediaRequestAddress (Channel.MediaDocument _) = "sendDocument"


mediaRequestData :: KeyValue kv => Channel.Media -> [kv]
mediaRequestData (Channel.MediaPhoto fileId) = ["photo" .= fileId]
mediaRequestData (Channel.MediaVideo fileId) = ["video" .= fileId]
mediaRequestData (Channel.MediaAudio fileId) = ["audio" .= fileId]
mediaRequestData (Channel.MediaAnimation fileId) = ["animation" .= fileId]
mediaRequestData (Channel.MediaVoice fileId) = ["voice" .= fileId]
mediaRequestData (Channel.MediaDocument fileId) = ["document" .= fileId]


tgcSendMediaGroup
    :: TgChannel
    -> Channel.ChatId
    -> Channel.MediaGroup
    -> IO (Either Text ())
tgcSendMediaGroup tgc chatId group = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Send media group"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show group)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "sendMediaGroup"])
        (object $
            [ "chat_id" .= chatId
            , "media" .= encodeMedia group
            ])
    case resp of
        TgResponseOk (TgVoid _) -> do
            Logger.debug (tgcLogger tgc) $
                "TgChannel: Media group sent"
            return $ Right ()
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Media group sending failed: " <> e
            return $ Left e


encodeMedia :: Channel.MediaGroup -> [Value]
encodeMedia (Channel.MediaGroupPhoto caption fileId next) = do
    (object $
        [ "type" .= ("photo" :: Text)
        , "media" .= fileId
        ] <> mediaCaptionOpt caption)
    :encodeMedia next
encodeMedia (Channel.MediaGroupVideo caption fileId next) = do
    (object $
        [ "type" .= ("video" :: Text)
        , "media" .= fileId
        ] <> mediaCaptionOpt caption)
    :encodeMedia next
encodeMedia Channel.MediaGroupEnd = []


tgcUpdateMessage
    :: TgChannel
    -> Channel.ChatId
    -> Channel.MessageId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text ())
tgcUpdateMessage tgc chatId messageId content buttons = do
    Logger.info (tgcLogger tgc) $
        "TgChannel: Update message"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show chatId) <> " <- " <> Text.pack (show messageId) <> " <- " <> Text.pack (show content)
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <> Text.pack (show buttons)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "editMessageText"])
        (object $
            [ "chat_id" .= chatId
            , "message_id" .= messageId
            ] <> richTextOpt content <> keyboardMarkupOpt (tgcKeyboardWidth tgc) buttons)
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


mediaCaptionOpt :: KeyValue kv => Text -> [kv]
mediaCaptionOpt "" = []
mediaCaptionOpt caption = ["caption" .= caption]


richTextOpt :: KeyValue kv => Channel.RichText -> [kv]
richTextOpt (Channel.RichTextSpan (Channel.SpanStyle False False False False) text Channel.RichTextEnd) =
    [ "text" .= text
    ]
richTextOpt rt =
    [ "text" .= Builder.toLazyText (encodeRichTextBuild rt)
    , "parse_mode" .= ("HTML" :: Text)
    ]


encodeRichTextBuild :: Channel.RichText -> Builder.Builder
encodeRichTextBuild (Channel.RichTextSpan (Channel.SpanStyle bold italic underline strike) text next) = do
    let mine =
            surround "<b>" "</b>" bold $
                surround "<i>" "</i>" italic $
                    surround "<u>" "</u>" underline $
                        surround "<s>" "</s>" strike $
                            escapeHtml text
    mine <> encodeRichTextBuild next
encodeRichTextBuild (Channel.RichTextLink href title next) = do
    "<a href=\"" <> escapeHtml href <> "\">" <> encodeRichTextBuild title <> "</a>" <> encodeRichTextBuild next
encodeRichTextBuild (Channel.RichTextMention user title next) = do
    "<a href=\"tg://user?id=" <> escapeHtml user <> "\">" <> encodeRichTextBuild title <> "</a>" <> encodeRichTextBuild next
encodeRichTextBuild (Channel.RichTextMono text next) = do
    "<code>" <> escapeHtml text <> "</code>" <> encodeRichTextBuild next
encodeRichTextBuild (Channel.RichTextCode "" text next) = do
    "<pre>" <> escapeHtml text <> "</pre>" <> encodeRichTextBuild next
encodeRichTextBuild (Channel.RichTextCode language text next) = do
    "<pre><code class=\"language-" <> escapeHtml language <> "\">" <> escapeHtml text <> "</code></pre>" <> encodeRichTextBuild next
encodeRichTextBuild Channel.RichTextEnd = mempty


surround :: Builder.Builder -> Builder.Builder -> Bool -> Builder.Builder -> Builder.Builder
surround left right True mid = left <> mid <> right
surround _ _ False mid = mid


escapeHtml :: Text -> Builder.Builder
escapeHtml = Text.foldr (\c a -> escapeHtmlChar c <> a) mempty


escapeHtmlChar :: Char -> Builder.Builder
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c = Builder.singleton c


data TgResponse a
    = TgResponseOk a
    | TgResponseErr Text


data TgGroupElem
    = TgGroupElem Channel.MediaGroupId Channel.ChatId (Channel.MediaGroup -> Channel.MediaGroup)


data TgUpdate
    = TgMessage
        { tguOffset :: !Integer
        , tguValue :: !Value }
    | TgQuery
        { tguOffset :: !Integer
        , tguValue :: !Value }
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


data TgEventMessage = TgEventMessage (Maybe Channel.MediaGroupId) Channel.Event


instance FromJSON TgEventMessage where
    parseJSON = withObject "TgMessage" $ \m -> do
        chatId <- m .: "chat" >>= (.: "id")
        mgroupId <- m .:? "media_group_id"
        msum
            [ parseMsgSticker chatId mgroupId m
            , parseMsgMedia chatId mgroupId m
            , parseMsgText chatId mgroupId m
            ]
        where
        parseMsgSticker chatId mgroupId m = do
            sticker <- m .: "sticker" >>= (.: "file_id")
            return $ TgEventMessage mgroupId $ Channel.EventSticker
                { Channel.eChatId = chatId
                , Channel.eSticker = sticker }
        parseMsgMedia chatId mgroupId m = do
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
            return $ TgEventMessage mgroupId $ Channel.EventMedia
                { Channel.eChatId = chatId
                , Channel.eCaption = caption
                , Channel.eMedia = media }
        parseMsgText chatId mgroupId m = do
            messageId <- m .: "message_id"
            text <- m .: "text"
            entities <- m .:? "entities" .!= []
            return $ TgEventMessage mgroupId $ Channel.EventMessage
                { Channel.eChatId = chatId
                , Channel.eMessageId = messageId
                , Channel.eMessage = parseMessageText text entities }


data TgTextEntity
    = TgTextEntity !Int !Int !TgTextEntityKind


data TgTextEntityKind
    = TgTextSpan !Channel.SpanStyle
    | TgTextLink !Text
    | TgTextMention !Text
    | TgTextMono
    | TgTextCode !Text
    | TgTextIgnore


instance FromJSON TgTextEntity where
    parseJSON = withObject "TgTextEntity" $ \m -> do
        ofs <- m .: "offset"
        len <- m .: "length"
        entype <- m .: "type"
        enkind <- case entype :: Text of
            "bold" -> return $ TgTextSpan $ Channel.SpanStyle True False False False
            "italic" -> return $ TgTextSpan $ Channel.SpanStyle False True False False
            "underline" -> return $ TgTextSpan $ Channel.SpanStyle False False True False
            "strikethrough" -> return $ TgTextSpan $ Channel.SpanStyle False False False True
            "code" -> return $ TgTextMono
            "pre" -> TgTextCode <$> m .:? "language" .!= ""
            "text_link" -> TgTextLink <$> m .:? "url" .!= ""
            "text_mention" -> do
                user <- (m .: "user" >>= (.: "id")) `mplus` return ""
                return $ TgTextMention user
            _ -> return $ TgTextIgnore
        return $ TgTextEntity ofs (ofs + len) enkind


newtype TgEventGroupElem = TgEventGroupElem (Channel.MediaGroup -> Channel.MediaGroup)


instance FromJSON TgEventGroupElem where
    parseJSON = withObject "TgGroupElem" $ \m -> do
        caption <- m .:? "caption" .!= ""
        msum
            [ (m .: "photo" >>=) $ withArray "PhotoSizes" $ \sizes -> do
                Just size0 <- return $ sizes Vector.!? 0
                flip (withObject "Photo") size0 $ \photo -> do
                    (TgEventGroupElem . Channel.MediaGroupPhoto caption) <$> photo .: "file_id"
            , (TgEventGroupElem . Channel.MediaGroupVideo caption) <$> (m .: "video" >>= (.: "file_id"))
            ]


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


parseMessageText :: TextLazy.Text -> [TgTextEntity] -> Channel.RichText
parseMessageText text entities = do
    let sortedEntities = sortBy entityComparer entities
    let chars = markOffset 0 $ TextLazy.unpack text
    runConsumer consumeMessage (\_ _ _ buf -> buf Channel.RichTextEnd) sortedEntities chars id
    where
    entityComparer :: TgTextEntity -> TgTextEntity -> Ordering
    entityComparer (TgTextEntity begin1 end1 _) (TgTextEntity begin2 end2 _) = compare begin1 begin2 <> compare end2 end1

    markOffset :: Int -> String -> [(Char, Int)]
    markOffset _ [] = []
    markOffset pos (x:xs)
        | x < '\x10000' = (x, pos):markOffset (pos+1) xs
        | otherwise = (x, pos):markOffset (pos+2) xs


newtype MessageTextConsumer t = MessageTextConsumer
    { runConsumer :: forall r
        .  (t -> [TgTextEntity] -> [(Char, Int)] -> (Channel.RichText -> Channel.RichText) -> r)
        ->       [TgTextEntity] -> [(Char, Int)] -> (Channel.RichText -> Channel.RichText) -> r }


instance Functor MessageTextConsumer where
    fmap = liftM


instance Applicative MessageTextConsumer where
    pure = return
    (<*>) = ap


instance Monad MessageTextConsumer where
    return x = MessageTextConsumer $ \cont -> cont x
    m >>= f = MessageTextConsumer $ \cont -> runConsumer m (\x -> runConsumer (f x) cont)


popEntity :: MessageTextConsumer (Maybe TgTextEntity)
popEntity = MessageTextConsumer $ \cont ents -> do
    case ents of
        e:es -> cont (Just e) es
        [] -> cont Nothing ents


popEntityBefore :: Int -> MessageTextConsumer (Maybe TgTextEntity)
popEntityBefore end = MessageTextConsumer $ \cont ents -> do
    case ents of
        e@(TgTextEntity entBegin _ _):es | entBegin < end -> cont (Just e) es
        _ -> cont Nothing ents


packWith :: Int -> (Text -> Channel.RichText -> Channel.RichText) -> MessageTextConsumer ()
packWith end termf = MessageTextConsumer $ \cont ents chars buf -> do
    let (left, rest) = break (\(_, pos) -> pos >= end) chars
    case left of
        [] -> cont () ents rest buf
        _ -> cont () ents rest (buf . termf (Text.pack $ map fst $ left))


packFinalWith :: (Text -> Channel.RichText -> Channel.RichText) -> MessageTextConsumer ()
packFinalWith termf = MessageTextConsumer $ \cont ents chars buf -> do
    case chars of
        [] -> cont () ents [] buf
        _ -> cont () ents [] (buf . termf (Text.pack $ map fst $ chars))


embedConsumer :: (Channel.RichText -> Channel.RichText -> Channel.RichText) -> MessageTextConsumer () -> MessageTextConsumer ()
embedConsumer mapf inner = MessageTextConsumer $ \cont ents1 chars1 buf1 -> do
    runConsumer inner
        (\_ ents2 chars2 innerbuf -> cont () ents2 chars2 (buf1 . mapf (innerbuf Channel.RichTextEnd)))
        ents1 chars1 id


consumeMessage :: MessageTextConsumer ()
consumeMessage = do
    ment <- popEntity
    case ment of
        Just (TgTextEntity _ _ TgTextIgnore) -> consumeMessage
        Just (TgTextEntity begin end kind) -> do
            packWith begin $ Channel.RichTextSpan Channel.plainStyle
            case kind of
                TgTextSpan style -> consumeStyleSpan end style
                TgTextLink href -> embedConsumer (Channel.RichTextLink href) $ consumeStyleSpan end Channel.plainStyle
                TgTextMention user -> embedConsumer (Channel.RichTextMention user) $ consumeStyleSpan end Channel.plainStyle
                TgTextMono -> packWith end $ Channel.RichTextMono
                TgTextCode lang -> packWith end $ Channel.RichTextCode lang
            consumeMessage
        Nothing -> do
            packFinalWith $ Channel.RichTextSpan Channel.plainStyle


consumeStyleSpan :: Int -> Channel.SpanStyle -> MessageTextConsumer ()
consumeStyleSpan spanEnd style = do
    ment <- popEntityBefore spanEnd
    case ment of
        Just (TgTextEntity begin end kind) -> do
            case kind of
                TgTextSpan style2 -> do
                    packWith begin $ Channel.RichTextSpan style
                    consumeStyleSpan end $ commonStyle style style2
                _ -> return ()
            consumeStyleSpan spanEnd style
        Nothing -> do
            packWith spanEnd $ Channel.RichTextSpan style


commonStyle :: Channel.SpanStyle -> Channel.SpanStyle -> Channel.SpanStyle
commonStyle (Channel.SpanStyle b1 i1 u1 s1) (Channel.SpanStyle b2 i2 u2 s2) = do
    Channel.SpanStyle (b1 || b2) (i1 || i2) (u1 || u2) (s1 || s2)
