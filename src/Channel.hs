module Channel
    ( ChatId
    , MessageId
    , StickerName
    , QueryId
    , QueryUserdata
    , Event(..)
    , QueryButton(..)
    , Handle(..)
    ) where


import Data.Text


type ChatId = Integer
type MessageId = Integer
type StickerName = Text
type QueryId = Text
type QueryUserdata = Text


data Event
    = EventMessage
        { eChatId :: !ChatId
        , eMessageId :: !MessageId
        , eMessage :: !Text }
    | EventSticker
        { eChatId :: !ChatId
        , eSticker :: !StickerName }
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
        , sendSticker :: ChatId -> StickerName -> IO (Either Text ())
        , updateMessage :: ChatId -> MessageId -> Text -> [QueryButton] -> IO (Either Text ())
        , answerQuery :: QueryId -> Text -> IO (Either Text ()) }
