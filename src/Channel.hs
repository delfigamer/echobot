module Channel
    ( ChatId
    , MessageId
    , FileId
    , MediaGroupId
    , QueryId
    , QueryUserdata
    , MediaGroup(..)
    , Media(..)
    , Event(..)
    , QueryButton(..)
    , Handle(..)
    ) where


import Data.Text


type ChatId = Integer
type MessageId = Integer
type FileId = Text
type MediaGroupId = Text
type QueryId = Text
type QueryUserdata = Text


data MediaGroup
    = MediaGroupPhoto !Text !FileId !MediaGroup
    | MediaGroupVideo !Text !FileId !MediaGroup
    | MediaGroupLast
    deriving (Show, Eq)


data Media
    = MediaPhoto !FileId
    | MediaVideo !FileId
    | MediaAudio !FileId
    | MediaAnimation !FileId
    | MediaVoice !FileId
    | MediaDocument !FileId
    deriving (Show, Eq)


data Event
    = EventMessage
        { eChatId :: !ChatId
        , eMessageId :: !MessageId
        , eMessage :: !Text }
    | EventSticker
        { eChatId :: !ChatId
        , eSticker :: !FileId }
    | EventMedia
        { eChatId :: !ChatId
        , eCaption :: !Text
        , eMedia :: !Media }
    | EventMediaGroup
        { eChatId :: !ChatId
        , eGroupId :: !MediaGroupId
        , eGroup :: !MediaGroup }
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
        , sendMessage :: ChatId -> Text -> [QueryButton] -> IO (Either Text MessageId)
        , sendSticker :: ChatId -> FileId -> IO (Either Text ())
        , sendMedia :: ChatId -> Text -> Media -> IO (Either Text ())
        , sendMediaGroup :: ChatId -> MediaGroup -> IO (Either Text ())
        , updateMessage :: ChatId -> MessageId -> Text -> [QueryButton] -> IO (Either Text ())
        , answerQuery :: QueryId -> Text -> IO (Either Text ()) }
