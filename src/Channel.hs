{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel
    (
    ) where


import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Text hiding (foldr)
import Network.HTTP.Req
import qualified System.IO as IO
import qualified Logger


data Message
    = MessageText Text


data Event
    = EventMessage
        { eChatId :: Integer
        , eFromId :: Integer
        , eMessage :: Message }


data Handle
    = Handle
        { poll :: IO [Event] }


{--}


newtype HttpIO t
    = HttpIO
        { runHttpIO :: IO t }
    deriving (Functor, Applicative, Monad, MonadIO)


instance MonadHttp HttpIO where
    handleHttpException = HttpIO . throwIO


{--}


data TgChannel
    = TgChannel
        { tgcToken :: Text
        , tgcLogger :: Logger.Handle
        , tgcOffset :: Maybe Integer }


tgcNew :: Text -> Logger.Handle -> IO TgChannel
tgcNew token logger = do
    return $ TgChannel
        { tgcLogger = logger
        , tgcToken = token
        , tgcOffset = Nothing }


tgcGetMe :: TgChannel -> IO ()
tgcGetMe tgc = do
    resp <- runHttpIO $ req
        GET
        (https "api.telegram.org" /: tgcToken tgc /: "getMe")
        NoReqBody
        lbsResponse
        mempty
    print $ responseBody resp
    return ()


tgcGetUpdates :: TgChannel -> IO [Event]
tgcGetUpdates tgc = do
    resp <- runHttpIO $ req
        GET
        (https "api.telegram.org" /: tgcToken tgc /: "getUpdates")
        NoReqBody
        jsonResponse
        (tgcOffsetOption tgc)
    print $ (responseBody resp :: Value)
    return []


tgcOffsetOption :: (Monoid param, QueryParam param) => TgChannel -> param
tgcOffsetOption tgc = do
    case tgcOffset tgc of
        Nothing -> mempty
        Just offset -> "offset" =: offset


-- tgResponce :: Value


tt :: Text -> IO ()
tt token = do
    Logger.withStdLogger Logger.Debug $ \logger -> do
        tgc <- tgcNew token logger
        tgcGetMe tgc
        tgcGetUpdates tgc
        return ()
