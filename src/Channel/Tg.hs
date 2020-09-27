{-# LANGUAGE RankNTypes #-}

module Channel.Tg
    ( Config(..)
    , withTgChannel
    ) where

import qualified Channel
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as Vector
import qualified Logger
import qualified WebDriver

data Config =
    Config
        { cToken :: Text
        , cTimeout :: Integer
        , cKeyboardWidth :: Int
        }
    deriving (Show)

withTgChannel ::
       Config
    -> Logger.Handle
    -> WebDriver.Handle
    -> (Channel.Handle -> IO r)
    -> IO r
withTgChannel conf logger driver body = do
    tgc <- tgcNew conf logger driver
    body $
        Channel.Handle
            { Channel.poll = tgcPoll tgc
            , Channel.sendMessage = tgcSendMessage tgc
            , Channel.sendMedia = tgcSendMedia tgc
            , Channel.possessMedia = tgcPossessMedia tgc
            , Channel.updateMessage = tgcUpdateMessage tgc
            , Channel.answerQuery = tgcAnswerQuery tgc
            }

data TgChannel =
    TgChannel
        { tgcToken :: Text
        , tgcTimeout :: Integer
        , tgcKeyboardWidth :: Int
        , tgcLogger :: Logger.Handle
        , tgcDriver :: WebDriver.Handle
        , tgcOffset :: IORef Integer
        }

tgcNew :: Config -> Logger.Handle -> WebDriver.Handle -> IO TgChannel
tgcNew conf logger driver = do
    unless (Text.all isValidTokenChar $ cToken conf) $ fail "Invalid token"
    poffset <- newIORef $! -1
    return $
        TgChannel
            { tgcToken = cToken conf
            , tgcTimeout = cTimeout conf
            , tgcKeyboardWidth = cKeyboardWidth conf
            , tgcLogger = logger
            , tgcDriver = driver
            , tgcOffset = poffset
            }
  where
    isValidTokenChar c =
        or
            [ '0' <= c && c <= '9'
            , 'a' <= c && c <= 'z'
            , 'A' <= c && c <= 'Z'
            , c == '_'
            , c == ':'
            ]

tgcPoll :: TgChannel -> IO [Channel.Event]
tgcPoll tgc = do
    oldoffset <- readIORef (tgcOffset tgc)
    Logger.debug (tgcLogger tgc) $
        "TgChannel: Current update offset: " <> Text.pack (show oldoffset)
    Logger.info (tgcLogger tgc) $ "TgChannel: Poll..."
    let offsetopt = [WebDriver.ParamNum "offset" $ oldoffset | oldoffset >= 0]
    resp <-
        WebDriver.request
            (tgcDriver tgc)
            ("https://api.telegram.org/" <> tgcToken tgc <> "/getUpdates") $
        [WebDriver.ParamNum "timeout" $ tgcTimeout tgc] <> offsetopt
    case resp of
        TgResponseOk tgupdates -> do
            Logger.info (tgcLogger tgc) $ "TgChannel: Response received"
            let newoffset =
                    maximum $
                    oldoffset : map (\tgu -> tguOffset tgu + 1) tgupdates
            writeIORef (tgcOffset tgc) $! newoffset
            Logger.debug (tgcLogger tgc) $
                "TgChannel: New update offset: " <> Text.pack (show newoffset)
            events <- parseUpdates tgupdates
            return $ groupEvents events
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $ "TgChannel: Request failed: " <> e
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
    parseUpdates :: [TgUpdate] -> IO [(Maybe Text.Text, Channel.Event)]
    parseUpdates [] = return $ []
    parseUpdates (tgu:rest) = do
        case tgu of
            TgMessage _ value -> do
                parseOrWarnOne "message" value (parseUpdates rest) $ \(TgEventMessage mgroupId ev) -> do
                    ((mgroupId, ev) :) <$> parseUpdates rest
            TgQuery _ value -> do
                parseOrWarnOne "callback query" value (parseUpdates rest) $ \(TgEventQuery ev) -> do
                    ((Nothing, ev) :) <$> parseUpdates rest
            TgUnknown _ fields -> do
                Logger.info (tgcLogger tgc) $ "TgChannel: Unknown update type:"
                Logger.info (tgcLogger tgc) $
                    "TgChannel: \t(w/fields) " <> Text.pack (show fields)
                parseUpdates rest
    parseOrWarnOne :: FromJSON a => Text -> Value -> IO r -> (a -> IO r) -> IO r
    parseOrWarnOne what value onError onSuccess = do
        case fromJSON value of
            Success x -> do
                onSuccess x
            Error e -> do
                Logger.warn (tgcLogger tgc) $ "TgChannel: " <> Text.pack e
                Logger.warn (tgcLogger tgc) $
                    "TgChannel: Unknown " <> what <> " type:"
                case value of
                    Object v -> do
                        Logger.warn (tgcLogger tgc) $
                            "TgChannel: \t(w/fields) " <>
                            Text.pack (show (HashMap.keys v))
                    Array _ -> do
                        Logger.warn (tgcLogger tgc) $ "TgChannel: \t(array)"
                    _ -> do
                        Logger.warn (tgcLogger tgc) $
                            "TgChannel: \t" <> Text.pack (show value)
                onError

groupEvents :: [(Maybe Text.Text, Channel.Event)] -> [Channel.Event]
groupEvents ((Just groupId1, ev1):rest1)
    | Channel.EventMedia chatId1 caption1 media1 <- ev1
    , (Just groupId2, Channel.EventMedia chatId2 caption2 media2):rest2 <- rest1
    , groupId1 == groupId2
    , chatId1 == chatId2
    , Text.null caption2 =
        groupEvents
            (( Just groupId1
             , Channel.EventMedia chatId1 caption1 (media1 ++ media2)) :
             rest2)
    | otherwise = ev1 : groupEvents rest1
groupEvents ((_, ev):rest) = ev : groupEvents rest
groupEvents [] = []

tgcSendMessage ::
       TgChannel
    -> Channel.ChatId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text Channel.MessageId)
tgcSendMessage tgc chatId content buttons = do
    Logger.info (tgcLogger tgc) $ "TgChannel: Send message"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <>
        Text.pack (show chatId) <> " <- " <> Text.pack (show content)
    Logger.debug (tgcLogger tgc) $ "TgChannel: \t" <> Text.pack (show buttons)
    resp <-
        WebDriver.request
            (tgcDriver tgc)
            ("https://api.telegram.org/" <> tgcToken tgc <> "/sendMessage") $
        [WebDriver.ParamNum "chat_id" $ chatId] <>
        richTextOpt content <> keyboardMarkupOpt (tgcKeyboardWidth tgc) buttons
    case resp of
        TgResponseOk (TgSentMessageId messageId) -> do
            Logger.debug (tgcLogger tgc) $
                "TgChannel: Message sent: " <> Text.pack (show messageId)
            return $ Right messageId
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Message sending failed: " <> e
            return $ Left e

tgcSendMedia ::
       TgChannel
    -> Channel.ChatId
    -> Text.Text
    -> [Channel.SendableMedia]
    -> IO (Either Text.Text ())
tgcSendMedia tgc chatId caption0 mediaList = do
    Logger.info (tgcLogger tgc) $ "TgChannel: Send media"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <>
        Text.pack (show chatId) <> " <- " <> Text.pack (show caption0)
    Logger.debug (tgcLogger tgc) $ "TgChannel: \t" <> Text.pack (show mediaList)
    let (trivialLeft, rest) = break isNontrivialMedia mediaList
    doTrivial (Right ()) caption0 trivialLeft rest
  where
    doTrivial current "" [] rest = doNext current rest
    doTrivial current caption subgroup rest = do
        Logger.debug (tgcLogger tgc) $
            "TgChannel: Send trivial: " <> Text.pack (show subgroup)
        let captionOpt =
                [ WebDriver.ParamText "caption" $ caption
                | not $ Text.null caption
                ]
        resp <-
            case subgroup of
                [Channel.SendableMedia Channel.MediaPhoto mediaId] -> do
                    WebDriver.request
                        (tgcDriver tgc)
                        ("https://api.telegram.org/" <>
                         tgcToken tgc <> "/sendPhoto") $
                        [ WebDriver.ParamNum "chat_id" $ chatId
                        , WebDriver.ParamText "photo" $ mediaId
                        ] <>
                        captionOpt
                [Channel.SendableMedia Channel.MediaVideo mediaId] -> do
                    WebDriver.request
                        (tgcDriver tgc)
                        ("https://api.telegram.org/" <>
                         tgcToken tgc <> "/sendVideo") $
                        [ WebDriver.ParamNum "chat_id" $ chatId
                        , WebDriver.ParamText "video" $ mediaId
                        ] <>
                        captionOpt
                _ -> do
                    WebDriver.request
                        (tgcDriver tgc)
                        ("https://api.telegram.org/" <>
                         tgcToken tgc <> "/sendMediaGroup") $
                        [ WebDriver.ParamNum "chat_id" $ chatId
                        , WebDriver.ParamJson "media" $
                          toJSON $ encodeMediaGroup caption subgroup
                        ]
        case resp of
            TgResponseOk (TgVoid _) -> do
                doNext current rest
            TgResponseErr e -> do
                Logger.err (tgcLogger tgc) $
                    "TgChannel: Media sending failed: " <> e
                doNext (current >> Left e) rest
    doSingular current (media@(Channel.SendableMedia mediaType mediaId):rest) = do
        Logger.debug (tgcLogger tgc) $
            "TgChannel: Send singular: " <> Text.pack (show media)
        let (methodUrl, paramName) =
                case mediaType of
                    Channel.MediaAudio -> ("/sendAudio", "audio")
                    Channel.MediaAnimation -> ("/sendAnimation", "animation")
                    Channel.MediaVoice -> ("/sendVoice", "voice")
                    Channel.MediaSticker -> ("/sendSticker", "sticker")
                    Channel.MediaDocument -> ("/sendDocument", "document")
                    _ -> error "shouldn't happen"
        resp <-
            WebDriver.request
                (tgcDriver tgc)
                ("https://api.telegram.org/" <> tgcToken tgc <> methodUrl) $
            [ WebDriver.ParamNum "chat_id" $ chatId
            , WebDriver.ParamText paramName $ mediaId
            ]
        case resp of
            TgResponseOk (TgVoid _) -> do
                doNext current rest
            TgResponseErr e -> do
                Logger.err (tgcLogger tgc) $
                    "TgChannel: Sticker sending failed: " <> e
                doNext (current >> Left e) rest
    doSingular _ [] = error "shouldn't happen"
    doNext current [] = return current
    doNext current subgroup = do
        let (trivialLeft, rest) = break isNontrivialMedia subgroup
        case trivialLeft of
            [] -> doSingular current rest
            _ -> doTrivial current "" trivialLeft rest
    isNontrivialMedia (Channel.SendableMedia Channel.MediaPhoto _) = False
    isNontrivialMedia (Channel.SendableMedia Channel.MediaVideo _) = False
    isNontrivialMedia _ = True

encodeMediaGroup :: Text.Text -> [Channel.SendableMedia] -> [Value]
encodeMediaGroup caption (Channel.SendableMedia mediaType mediaId:rest) = do
    let captionOpt = ["caption" .= caption | not $ Text.null caption]
    let typename =
            case mediaType of
                Channel.MediaPhoto -> "photo"
                Channel.MediaVideo -> "video"
                _ -> error "shouldn't happen"
    let me =
            object $
            ["type" .= (typename :: Text), "media" .= mediaId] <> captionOpt
    me : encodeMediaGroup "" rest
encodeMediaGroup _ [] = []

tgcPossessMedia ::
       TgChannel
    -> Channel.ChatId
    -> Channel.ForeignMedia
    -> IO Channel.PossessMediaOutcome
tgcPossessMedia _ _ (Channel.ForeignMedia mediaType mediaId _) = do
    case mediaType of
        Channel.MediaUnknown ->
            return $ Channel.PossessMediaUnknownType $ mediaId
        _ ->
            return $
            Channel.PossessMediaSuccess $
            Channel.SendableMedia mediaType mediaId

tgcUpdateMessage ::
       TgChannel
    -> Channel.ChatId
    -> Channel.MessageId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text ())
tgcUpdateMessage tgc chatId messageId content buttons = do
    Logger.info (tgcLogger tgc) $ "TgChannel: Update message"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <>
        Text.pack (show chatId) <>
        " <- " <>
        Text.pack (show messageId) <> " <- " <> Text.pack (show content)
    Logger.debug (tgcLogger tgc) $ "TgChannel: \t" <> Text.pack (show buttons)
    resp <-
        WebDriver.request
            (tgcDriver tgc)
            ("https://api.telegram.org/" <> tgcToken tgc <> "/editMessageText") $
        [ WebDriver.ParamNum "chat_id" $ chatId
        , WebDriver.ParamNum "message_id" $ messageId
        ] <>
        richTextOpt content <> keyboardMarkupOpt (tgcKeyboardWidth tgc) buttons
    case resp of
        TgResponseOk (TgVoid _) -> do
            Logger.debug (tgcLogger tgc) $ "TgChannel: Message updated"
            return $ Right ()
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $
                "TgChannel: Message update failed: " <> e
            return $ Left e

tgcAnswerQuery :: TgChannel -> Channel.QueryId -> Text -> IO (Either Text ())
tgcAnswerQuery tgc queryId text = do
    Logger.info (tgcLogger tgc) $ "TgChannel: Answer query"
    Logger.debug (tgcLogger tgc) $
        "TgChannel: \t" <>
        Text.pack (show queryId) <> " <- " <> Text.pack (show text)
    resp <-
        WebDriver.request
            (tgcDriver tgc)
            ("https://api.telegram.org/" <>
             tgcToken tgc <> "/answerCallbackQuery") $
        [ WebDriver.ParamText "callback_query_id" $ queryId
        , WebDriver.ParamText "text" $ text
        ]
    case resp of
        TgResponseOk (TgVoid _) -> do
            Logger.debug (tgcLogger tgc) $ "TgChannel: Query answered"
            return $ Right ()
        TgResponseErr e -> do
            Logger.err (tgcLogger tgc) $ "TgChannel: Query answer failed: " <> e
            return $ Left e

keyboardMarkupOpt :: Int -> [Channel.QueryButton] -> [WebDriver.Param]
keyboardMarkupOpt _ [] = []
keyboardMarkupOpt kbwidth buttons = do
    let buttonvalues =
            map
                (\(Channel.QueryButton title userdata) -> do
                     object ["text" .= title, "callback_data" .= userdata])
                buttons
    let value = object ["inline_keyboard" .= sectionList kbwidth buttonvalues]
    [WebDriver.ParamJson "reply_markup" $ value]
  where
    sectionList len xs = do
        case splitAt len xs of
            (_, []) -> [xs]
            (h, r) -> h : sectionList len r

richTextOpt :: Channel.RichText -> [WebDriver.Param]
richTextOpt (Channel.RichTextSpan (Channel.SpanStyle False False False False) text Channel.RichTextEnd) =
    [WebDriver.ParamText "text" $ text]
richTextOpt rt =
    [ WebDriver.ParamTextLazy "text" $ Builder.toLazyText $ encodeRichText rt
    , WebDriver.ParamText "parse_mode" $ "HTML"
    ]

encodeRichText :: Channel.RichText -> Builder.Builder
encodeRichText (Channel.RichTextSpan (Channel.SpanStyle bold italic underline strike) text next) = do
    let mine =
            surround "<b>" "</b>" bold $
            surround "<i>" "</i>" italic $
            surround "<u>" "</u>" underline $
            surround "<s>" "</s>" strike $ escapeHtml text
    mine <> encodeRichText next
encodeRichText (Channel.RichTextLink href title next) = do
    "<a href=\"" <>
        escapeHtml href <>
        "\">" <> encodeRichText title <> "</a>" <> encodeRichText next
encodeRichText (Channel.RichTextMention user title next) = do
    "<a href=\"tg://user?id=" <>
        escapeHtml user <>
        "\">" <> encodeRichText title <> "</a>" <> encodeRichText next
encodeRichText (Channel.RichTextMono text next) = do
    "<code>" <> escapeHtml text <> "</code>" <> encodeRichText next
encodeRichText (Channel.RichTextCode "" text next) = do
    "<pre>" <> escapeHtml text <> "</pre>" <> encodeRichText next
encodeRichText (Channel.RichTextCode language text next) = do
    "<pre><code class=\"language-" <>
        escapeHtml language <>
        "\">" <> escapeHtml text <> "</code></pre>" <> encodeRichText next
encodeRichText Channel.RichTextEnd = mempty

surround ::
       Builder.Builder
    -> Builder.Builder
    -> Bool
    -> Builder.Builder
    -> Builder.Builder
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

data TgUpdate
    = TgMessage !Integer !Value
    | TgQuery !Integer !Value
    | TgUnknown !Integer [Text]

tguOffset :: TgUpdate -> Integer
tguOffset (TgMessage ofs _) = ofs
tguOffset (TgQuery ofs _) = ofs
tguOffset (TgUnknown ofs _) = ofs

instance FromJSON a => FromJSON (TgResponse a) where
    parseJSON =
        withObject "TgResponse" $ \v -> do
            isOk <- v .: "ok"
            if isOk
                then TgResponseOk <$> v .: "result"
                else TgResponseErr <$> v .: "description"

instance FromJSON TgUpdate where
    parseJSON =
        withObject "TgEvent" $ \v -> do
            updateId <- v .: "update_id"
            msum
                [ TgMessage updateId <$> v .: "message"
                , TgQuery updateId <$> v .: "callback_query"
                , return $ TgUnknown updateId (HashMap.keys v)
                ]

data TgEventMessage =
    TgEventMessage (Maybe Text.Text) Channel.Event

instance FromJSON TgEventMessage where
    parseJSON =
        withObject "TgMessage" $ \m -> do
            chatId <- m .: "chat" >>= (.: "id")
            mgroupId <- m .:? "media_group_id"
            msum
                [ parseMsgMedia chatId mgroupId m
                , parseMsgText chatId mgroupId m
                ]
      where
        parseMsgMedia chatId mgroupId m = do
            caption <- m .:? "caption" .!= ""
            media <-
                msum
                    [ (m .: "photo" >>=) $
                      withArray "PhotoSizes" $ \sizes -> do
                          Just size0 <- return $ sizes Vector.!? 0
                          flip (withObject "Photo") size0 $ \photo -> do
                              makeMedia Channel.MediaPhoto <$>
                                  photo .: "file_id"
                    , makeMedia Channel.MediaVideo <$>
                      (m .: "video" >>= (.: "file_id"))
                    , makeMedia Channel.MediaAudio <$>
                      (m .: "audio" >>= (.: "file_id"))
                    , makeMedia Channel.MediaAnimation <$>
                      (m .: "animation" >>= (.: "file_id"))
                    , makeMedia Channel.MediaVoice <$>
                      (m .: "voice" >>= (.: "file_id"))
                    , makeMedia Channel.MediaSticker <$>
                      (m .: "sticker" >>= (.: "file_id"))
                    , makeMedia Channel.MediaDocument <$>
                      (m .: "document" >>= (.: "file_id"))
                    ]
            return $
                TgEventMessage mgroupId $
                Channel.EventMedia
                    { Channel.eChatId = chatId
                    , Channel.eCaption = caption
                    , Channel.eMedia = [media]
                    }
        parseMsgText chatId mgroupId m = do
            messageId <- m .: "message_id"
            text <- m .: "text"
            entities <- m .:? "entities" .!= []
            return $
                TgEventMessage mgroupId $
                Channel.EventMessage
                    { Channel.eChatId = chatId
                    , Channel.eMessageId = messageId
                    , Channel.eMessage = parseMessageText text entities
                    }
        makeMedia mediaType mediaId = Channel.ForeignMedia mediaType mediaId ""

data TgTextEntity =
    TgTextEntity !Int !Int !TgTextEntityKind

data TgTextEntityKind
    = TgTextSpan !Channel.SpanStyle
    | TgTextLink !Text
    | TgTextMention !Text
    | TgTextMono
    | TgTextCode !Text
    | TgTextIgnore

instance FromJSON TgTextEntity where
    parseJSON =
        withObject "TgTextEntity" $ \m -> do
            ofs <- m .: "offset"
            len <- m .: "length"
            entype <- m .: "type"
            enkind <-
                case entype :: Text of
                    "bold" ->
                        return $
                        TgTextSpan $ Channel.SpanStyle True False False False
                    "italic" ->
                        return $
                        TgTextSpan $ Channel.SpanStyle False True False False
                    "underline" ->
                        return $
                        TgTextSpan $ Channel.SpanStyle False False True False
                    "strikethrough" ->
                        return $
                        TgTextSpan $ Channel.SpanStyle False False False True
                    "code" -> return $ TgTextMono
                    "pre" -> TgTextCode <$> m .:? "language" .!= ""
                    "text_link" -> TgTextLink <$> m .:? "url" .!= ""
                    "text_mention" -> do
                        user <- (m .: "user" >>= (.: "id")) `mplus` return ""
                        return $ TgTextMention user
                    _ -> return $ TgTextIgnore
            return $ TgTextEntity ofs (ofs + len) enkind

newtype TgEventQuery =
    TgEventQuery Channel.Event

instance FromJSON TgEventQuery where
    parseJSON =
        withObject "TgCallbackQuery" $ \e -> do
            queryId <- e .: "id"
            userdata <- e .: "data"
            (e .: "message" >>=) $
                withObject "TgMessage" $ \m -> do
                    chatId <- m .: "chat" >>= (.: "id")
                    messageId <- m .: "message_id"
                    return $
                        TgEventQuery $
                        Channel.EventQuery
                            { Channel.eChatId = chatId
                            , Channel.eMessageId = messageId
                            , Channel.eQueryId = queryId
                            , Channel.eUserdata = userdata
                            }

newtype TgSentMessageId =
    TgSentMessageId Channel.MessageId

instance FromJSON TgSentMessageId where
    parseJSON =
        withObject "TgSentMessage" $ \v -> do
            TgSentMessageId <$> v .: "message_id"

newtype TgVoid =
    TgVoid ()

instance FromJSON TgVoid where
    parseJSON _ = return $ TgVoid ()

parseMessageText :: TextLazy.Text -> [TgTextEntity] -> Channel.RichText
parseMessageText text entities = do
    let sortedEntities = sortBy entityComparer entities
    let chars = markOffset 0 $ TextLazy.unpack text
    runConsumer
        consumeMessage
        (\_ _ _ buf -> buf Channel.RichTextEnd)
        sortedEntities
        chars
        id
  where
    entityComparer :: TgTextEntity -> TgTextEntity -> Ordering
    entityComparer (TgTextEntity begin1 end1 _) (TgTextEntity begin2 end2 _) =
        compare begin1 begin2 <> compare end2 end1
    markOffset :: Int -> String -> [(Char, Int)]
    markOffset _ [] = []
    markOffset pos (x:xs)
        | x < '\x10000' = (x, pos) : markOffset (pos + 1) xs
        | otherwise = (x, pos) : markOffset (pos + 2) xs

newtype MessageTextConsumer t =
    MessageTextConsumer
        { runConsumer :: forall r. (t -> [TgTextEntity] -> [(Char, Int)] -> (Channel.RichText -> Channel.RichText) -> r) -> [TgTextEntity] -> [( Char
                                                                                                                                               , Int)] -> (Channel.RichText -> Channel.RichText) -> r
        }

instance Functor MessageTextConsumer where
    fmap = liftM

instance Applicative MessageTextConsumer where
    pure = return
    (<*>) = ap

instance Monad MessageTextConsumer where
    return x = MessageTextConsumer $ \cont -> cont x
    m >>= f =
        MessageTextConsumer $ \cont ->
            runConsumer m (\x -> runConsumer (f x) cont)

popEntity :: MessageTextConsumer (Maybe TgTextEntity)
popEntity =
    MessageTextConsumer $ \cont ents -> do
        case ents of
            e:es -> cont (Just e) es
            [] -> cont Nothing ents

popEntityBefore :: Int -> MessageTextConsumer (Maybe TgTextEntity)
popEntityBefore end =
    MessageTextConsumer $ \cont ents -> do
        case ents of
            e@(TgTextEntity entBegin _ _):es
                | entBegin < end -> cont (Just e) es
            _ -> cont Nothing ents

packWith ::
       Int
    -> (Text -> Channel.RichText -> Channel.RichText)
    -> MessageTextConsumer ()
packWith end termf =
    MessageTextConsumer $ \cont ents chars buf -> do
        let (left, rest) = break (\(_, pos) -> pos >= end) chars
        case left of
            [] -> cont () ents rest buf
            _ -> cont () ents rest (buf . termf (Text.pack $ map fst $ left))

packFinalWith ::
       (Text -> Channel.RichText -> Channel.RichText) -> MessageTextConsumer ()
packFinalWith termf =
    MessageTextConsumer $ \cont ents chars buf -> do
        case chars of
            [] -> cont () ents [] buf
            _ -> cont () ents [] (buf . termf (Text.pack $ map fst $ chars))

embedConsumer ::
       (Channel.RichText -> Channel.RichText -> Channel.RichText)
    -> MessageTextConsumer ()
    -> MessageTextConsumer ()
embedConsumer mapf inner =
    MessageTextConsumer $ \cont ents1 chars1 buf1 -> do
        runConsumer
            inner
            (\_ ents2 chars2 innerbuf ->
                 cont
                     ()
                     ents2
                     chars2
                     (buf1 . mapf (innerbuf Channel.RichTextEnd)))
            ents1
            chars1
            id

consumeMessage :: MessageTextConsumer ()
consumeMessage = do
    ment <- popEntity
    case ment of
        Just (TgTextEntity _ _ TgTextIgnore) -> consumeMessage
        Just (TgTextEntity begin end kind) -> do
            packWith begin $ Channel.RichTextSpan Channel.plainStyle
            case kind of
                TgTextSpan style -> consumeStyleSpan end style
                TgTextLink href ->
                    embedConsumer (Channel.RichTextLink href) $
                    consumeStyleSpan end Channel.plainStyle
                TgTextMention user ->
                    embedConsumer (Channel.RichTextMention user) $
                    consumeStyleSpan end Channel.plainStyle
                TgTextMono -> packWith end $ Channel.RichTextMono
                TgTextCode lang -> packWith end $ Channel.RichTextCode lang
                TgTextIgnore -> error "shouldn't happen"
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
