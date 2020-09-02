module Channel
    ( Message(..)
    , Event(..)
    , Handle(..)
    ) where


import Data.Text


data Message
    = MessageText !Text
    | MessageSticker !Text
    deriving (Show, Eq)


data Event
    = EventMessage
        { eChatId :: !Integer
        , eFromId :: !Integer
        , eMessage :: !Message }
    deriving (Show, Eq)


data Handle
    = Handle
        { poll :: IO [Event] }
