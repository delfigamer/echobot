module Channel
    ( ChatId
    , MessageId
    , FileId
    , MediaGroupId
    , QueryId
    , QueryUserdata
    , SpanStyle(..)
    , RichText(..)
    , MediaType(..)
    , ForeignMedia(..)
    , SendableMedia(..)
    , Event(..)
    , QueryButton(..)
    , Handle(..)
    , plainText
    , plainStyle
    ) where


import Data.Text


type ChatId = Integer
type MessageId = Integer
type FileId = Text
type MediaGroupId = Text
type QueryId = Text
type QueryUserdata = Text


data SpanStyle
    = SpanStyle
        { ssBold :: !Bool
        , ssItalic :: !Bool
        , ssUnderline :: !Bool
        , ssStrike :: !Bool
        }
    deriving (Show, Eq)


data RichText
    = RichTextSpan !SpanStyle !Text !RichText
    | RichTextLink !Text !RichText !RichText
    | RichTextMention !Text !RichText !RichText
    | RichTextMono !Text !RichText
    | RichTextCode !Text !Text !RichText
    | RichTextEnd
    deriving (Show, Eq)


data MediaType
    = MediaPhoto
    | MediaVideo
    | MediaAudio
    | MediaAnimation
    | MediaVoice
    | MediaSticker
    | MediaDocument
    | MediaUnknown
    deriving (Show, Eq)


data ForeignMedia
    = ForeignMedia !MediaType !Text !Text
    deriving (Show, Eq)


data SendableMedia
    = SendableMedia !MediaType !Text
    deriving (Show, Eq)


data Event
    = EventMessage
        { eChatId :: !ChatId
        , eMessageId :: !MessageId
        , eMessage :: !RichText }
    | EventMedia
        { eChatId :: !ChatId
        , eCaption :: !Text
        , eMedia :: [ForeignMedia] }
    | EventQuery
        { eChatId :: !ChatId
        , eMessageId :: !MessageId
        , eQueryId :: !QueryId
        , eUserdata :: !QueryUserdata }
    deriving (Show, Eq)


data QueryButton
    = QueryButton
        { bTitle :: !Text
        , bUserdata :: !QueryUserdata }
    deriving (Show, Eq)


data Handle
    = Handle
        { poll :: IO [Event]
        , sendMessage :: ChatId -> RichText -> [QueryButton] -> IO (Either Text ())
        , sendMedia :: ChatId -> Text -> [SendableMedia] -> IO (Either Text ())
        , possessMedia :: ForeignMedia -> IO (Either Text SendableMedia)
        , updateMessage :: ChatId -> MessageId -> RichText -> [QueryButton] -> IO (Either Text ())
        , answerQuery :: QueryId -> Text -> IO (Either Text ()) }


plainText :: Text -> RichText
plainText text = RichTextSpan plainStyle text RichTextEnd


plainStyle :: SpanStyle
plainStyle = SpanStyle False False False False
