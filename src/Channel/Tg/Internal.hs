{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel.Tg.Internal
    ( Config(..)
    , TgChannel(..)
    , tgcNew
    , tgcPoll
    ) where


import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Channel
import qualified Logger
import qualified WebDriver


data Config
    = Config
        { cToken :: Text
        , cTimeout :: Int }


withTgChannel :: Config -> Logger.Handle -> WebDriver.Handle -> (Channel.Handle -> IO r) -> IO r
withTgChannel conf logger driver body = do
    tgc <- tgcNew conf logger driver
    body $ Channel.Handle
        { Channel.poll = tgcPoll tgc }


data TgChannel
    = TgChannel
        { tgcToken :: Text
        , tgcTimeout :: Int
        , tgcLogger :: Logger.Handle
        , tgcDriver :: WebDriver.Handle
        , tgcOffset :: IORef Integer }


tgcNew :: Config -> Logger.Handle -> WebDriver.Handle -> IO TgChannel
tgcNew conf logger driver = do
    poffset <- newIORef $! -1
    return $ TgChannel
        { tgcToken = cToken conf
        , tgcTimeout = cTimeout conf
        , tgcLogger = logger
        , tgcDriver = driver
        , tgcOffset = poffset }


tgcPoll :: TgChannel -> IO [Channel.Event]
tgcPoll tgc = do
    oldoffset <- readIORef (tgcOffset tgc)
    let offsetopt = if oldoffset < 0
        then id
        else (("offset" .= oldoffset):)
    resp <- WebDriver.request (tgcDriver tgc)
        (WebDriver.HttpsAddress "api.telegram.org" [tgcToken tgc, "getUpdates"])
        (object $ offsetopt ["timeout" .= tgcTimeout tgc])
    case resp of
        TgResponseOk tgevents -> do
            let newoffset = maximum $
                    oldoffset
                    :map (\(TgEvent myoffset _) -> myoffset + 1) tgevents
            writeIORef (tgcOffset tgc) $! newoffset
            return $ catMaybes $ map (\(TgEvent _ myev) -> myev) tgevents
        TgResponseErr e -> do
            undefined


data TgResponse
    = TgResponseOk [TgEvent]
    | TgResponseErr Text
    deriving (Show)


data TgEvent = TgEvent !Integer (Maybe Channel.Event)
    deriving (Show)


instance FromJSON TgResponse where
    parseJSON = withObject "TgResponse" $ \v -> do
        isOk <- v .: "ok"
        if isOk
            then do
                TgResponseOk
                    <$> v .: "result"
            else do
                TgResponseErr
                    <$> v .: "description"


instance FromJSON TgEvent where
    parseJSON = withObject "TgEvent" $ \v -> do
        updateId <- v .: "update_id"
        event <- optional $ parseEventMsg v
        return $ TgEvent updateId event
        where
        parseEventMsg v = (v .: "message" >>=) $ withObject "Event" $ \e -> do
            chatId <- e .: "chat" >>= (.: "id")
            fromId <- e .: "from" >>= (.: "id")
            message <- parseMsgSticker e <|> parseMsgText e
            return $ Channel.EventMessage
                { Channel.eChatId = chatId
                , Channel.eFromId = fromId
                , Channel.eMessage = message }
        parseMsgSticker e = do
            Channel.MessageSticker
                <$> (e .: "sticker" >>= (.: "file_id"))
        parseMsgText e = do
            Channel.MessageText
                <$> (e .: "text")


-- tt :: Text -> IO ()
-- tt token = do
    -- Logger.withStdLogger $ \logger -> do
        -- WebDriver.withWebDriver logger $ \driver -> do
            -- tgc <- tgcNew token logger driver
            -- tgcPoll tgc >>= print
            -- tgcPoll tgc >>= print
            -- tgcPoll tgc >>= print
            -- return ()
