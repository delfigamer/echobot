{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module WebDriver
    ( Address(..)
    , Handle(..)
    , withWebDriver
    , (=:)
    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Aeson.Text
import Network.HTTP.Req
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified System.IO as IO
import qualified Logger


data Address
    = HttpsAddress Text.Text [Text.Text]
    deriving (Show, Eq)


data Handle
    = Handle
        { request :: forall a b . (ToJSON a, FromJSON b) => Address -> a -> IO b
        , download :: Address -> [(Text.Text, Text.Text)] -> IO BSL.ByteString
        , upload :: forall b . (FromJSON b) => Address -> BSL.ByteString -> IO b }


{--}


withWebDriver :: Logger.Handle -> (Handle -> IO r) -> IO r
withWebDriver logger body = do
    body $ Handle
        { request = webRequest logger
        , download = webDownload logger
        , upload = webUpload logger
        }


webRequest :: (ToJSON a, FromJSON b) => Logger.Handle -> Address -> a -> IO b
webRequest logger (HttpsAddress root nodes) params = do
    Logger.info logger $
        "WebDriver: Send request to " <> root
    Logger.debug logger $
        "WebDriver: \t" <> encodeToText params
    resp <- runReq defaultHttpConfig $ req
        POST
        (foldl (/:) (https root) nodes)
        (ReqBodyJson $ params)
        jsonResponse
        mempty
    let value = responseBody resp :: Value
    Logger.info logger $
        "WebDriver: Response received"
    webDecodeJson logger $ responseBody resp


webDownload :: Logger.Handle -> Address -> [(Text.Text, Text.Text)] -> IO BSL.ByteString
webDownload logger (HttpsAddress root nodes) qparams = do
    Logger.info logger $
        "WebDriver: Download a file from " <> root
    resp <- runReq defaultHttpConfig $ req
        GET
        (foldl (/:) (https root) nodes)
        NoReqBody
        lbsResponse
        (foldMap (uncurry (=:)) qparams)
    Logger.info logger $
        "WebDriver: Response received"
    return $ responseBody resp


webUpload :: FromJSON b => Logger.Handle -> Address -> BSL.ByteString -> IO b
webUpload logger (HttpsAddress root nodes) content = do
    Logger.info logger $
        "WebDriver: Upload a file to " <> root
    resp <- runReq defaultHttpConfig $ req
        POST
        (foldl (/:) (https root) nodes)
        (ReqBodyLbs content)
        jsonResponse
        mempty
    Logger.info logger $
        "WebDriver: Response received"
    webDecodeJson logger $ responseBody resp


webDecodeJson :: FromJSON b => Logger.Handle -> Value -> IO b
webDecodeJson logger value = do
    Logger.debug logger $
        "WebDriver: \t" <> encodeToText value
    case fromJSON value of
        Success x -> do
            Logger.debug logger $
                "WebDriver: Response successfully parsed"
            return $ x
        Error e -> do
            Logger.err logger $
                "WebDriver: Response failed to parse: " <> Text.pack e
            throwIO $ JsonHttpException e


encodeToText :: ToJSON a => a -> Text.Text
encodeToText = TextLazy.toStrict . encodeToLazyText
