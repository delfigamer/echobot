{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel.Tg.InternalSpec
    ( spec
    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Req
import Test.Hspec
import Test.Hspec.Expectations
import qualified System.IO as IO
import qualified Channel
import qualified Logger
import qualified WebDriver
import Channel.Tg.Internal


withTestDriver
    :: [Value]
    -> (WebDriver.Handle -> IO r)
    -> IO ([Value], [(WebDriver.Address, Value)], r)
withTestDriver seed body = do
    psource <- newIORef $ seed
    psink <- newIORef $ []
    result <- body $ WebDriver.Handle
        { WebDriver.request = testRequest psource psink }
    leftover <- readIORef psource
    reqs <- readIORef psink
    return $ (leftover, reqs, result)


testRequest
    :: (ToJSON a, FromJSON b)
    => IORef [Value]
    -> IORef [(WebDriver.Address, Value)]
    -> WebDriver.Address -> a -> IO b
testRequest psource psink address params = do
    modifyIORef psink $ ((address, toJSON params):)
    source <- readIORef psource
    case source of
        [] -> throwIO $ ErrorCall "too many requests"
        v:vs -> do
            writeIORef psource $! vs
            case fromJSON v of
                Error err -> throwIO $ ErrorCall err
                Success x -> return $ x


spec :: Spec
spec = do
    describe "Channel.Tg" $ do
        let token = "bottok"
        let timeout = 56
        let conf = Config
                { cToken = token
                , cTimeout = timeout }
        it "sends requests" $ do
            let seed = [
                    object $ ["ok" .= True, "result" .= Array mempty]]
            Logger.withNullLogger $ \logger -> do
                (leftover, reqs, result) <- withTestDriver seed $ \driver -> do
                    tgc <- tgcNew conf logger driver
                    tgcPoll tgc
                leftover `shouldBe` []
                reqs `shouldBe` [
                    ( WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"]
                    , object $ ["timeout" .= timeout])]
                result `shouldBe` []
        it "keeps track of the offset" $ do
            pending
        it "receives text and sticker messages" $ do
            pending
