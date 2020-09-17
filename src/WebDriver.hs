{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module WebDriver
    ( Address(..)
    , Part(..)
    , Handle(..)
    , withWebDriver
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
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as TextLazy
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified System.IO as IO
import qualified Text.URI as URI
import qualified Logger


data Address
    = HttpsAddress Text.Text [Text.Text]
    deriving (Show, Eq)


data Part
    = PartBSL !Text.Text !BSL.ByteString
    | PartText !Text.Text !Text.Text
    deriving (Show, Eq)


data Handle
    = Handle
        { request :: forall a b . (ToJSON a, FromJSON b) => Address -> a -> IO b
        , download :: Text.Text -> IO BSL.ByteString
        , upload :: forall b . (FromJSON b) => Text.Text -> [Part] -> IO b }


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


webDownload :: Logger.Handle -> Text.Text -> IO BSL.ByteString
webDownload logger address = do
    Logger.info logger $
        "WebDriver: Download a file from " <> address
    (url, options) <- parseUrlVerbose logger address
    resp <- runReq defaultHttpConfig $ req
        GET
        url
        NoReqBody
        lbsResponse
        options
    Logger.info logger $
        "WebDriver: Response received"
    return $ responseBody resp


webUpload :: FromJSON b => Logger.Handle -> Text.Text -> [Part] -> IO b
webUpload logger address parts = do
    Logger.info logger $
        "WebDriver: Upload a file to " <> address
    (url, options) <- parseUrlVerbose logger address
    body <- reqBodyMultipart $ map encodePart parts
    resp <- runReq defaultHttpConfig $ req
        POST
        url
        body
        jsonResponse
        options
    Logger.info logger $
        "WebDriver: Response received"
    webDecodeJson logger $ responseBody resp
    where
    encodePart (PartBSL name content) = Multipart.partLBS name content
    encodePart (PartText name text) = Multipart.partBS name $ Encoding.encodeUtf8 text


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


parseUrlVerbose :: Logger.Handle -> Text.Text -> IO (Url 'Https, Option scheme)
parseUrlVerbose logger source = do
    case URI.mkURI source >>= useHttpsURI of
        Nothing -> do
            Logger.err logger $
                "WebDriver: Invalid URI: " <> Text.pack (show source)
            fail $ "invalid URL"
        Just result -> return $ result


encodeToText :: ToJSON a => a -> Text.Text
encodeToText = TextLazy.toStrict . encodeToLazyText
