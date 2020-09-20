{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module WebDriver
    ( Address
    , Param(..)
    , Handle(..)
    , paramName
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
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as EncodingLazy
import qualified Network.HTTP.Client.MultipartFormData as Multipart
import qualified System.IO as IO
import qualified Text.URI as URI
import qualified Logger


type Address = Text.Text


data Param
    = ParamText !Text.Text !Text.Text
    | ParamTextLazy !Text.Text !TextLazy.Text
    | ParamNum !Text.Text !Integer
    | ParamJson !Text.Text !Value
    | ParamBytes !Text.Text !BSL.ByteString
    deriving (Show, Eq)


paramName (ParamText name _) = name
paramName (ParamTextLazy name _) = name
paramName (ParamNum name _) = name
paramName (ParamJson name _) = name
paramName (ParamBytes name _) = name


data Handle
    = Handle
        { request :: forall b . (FromJSON b) => Address -> [Param] -> IO b
        , download :: Address -> IO BSL.ByteString }


withWebDriver :: Logger.Handle -> (Handle -> IO r) -> IO r
withWebDriver logger body = do
    body $ Handle
        { request = webRequest logger
        , download = webDownload logger
        }


webRequest :: (FromJSON b) => Logger.Handle -> Address -> [Param] -> IO b
webRequest logger address params = do
    Logger.debug logger $
        "WebDriver: Send request to " <> address
    forM params $ \p -> do
        case p of
            ParamText name text -> do
                Logger.debug logger $
                    "WebDriver: \t" <> name <> " = \"" <> escapeText text <> "\""
            ParamTextLazy name text -> do
                Logger.debug logger $
                    "WebDriver: \t" <> name <> " = \"" <> TextLazy.toStrict (escapeTextLazy text) <> "\""
            ParamNum name number -> do
                Logger.debug logger $
                    "WebDriver: \t" <> name <> " = " <> Text.pack (show number)
            ParamJson name value -> do
                Logger.debug logger $
                    "WebDriver: \t" <> name <> " = " <> encodeToText value
            ParamBytes name payload -> do
                Logger.debug logger $
                    "WebDriver: \t" <> name <> " = <" <> Text.pack (show $ BSL.length payload) <> " bytes>"
    let hasParamBytes = flip any params $ \p -> do
        case p of
            ParamBytes _ _ -> True
            _ -> False
    (url, options) <- parseUrlVerbose logger address
    resp <- if hasParamBytes
        then do
            body <- reqBodyMultipart $ map multipart params
            runReq defaultHttpConfig $
                req POST url body jsonResponse options
        else do
            runReq defaultHttpConfig $
                req POST url (ReqBodyUrlEnc $ foldMap urlencoded params) jsonResponse options
    let value = responseBody resp
    Logger.debug logger $
        "WebDriver: Response received"
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
            fail "invalid response"
    where
    urlencoded (ParamText name text) = name =: text
    urlencoded (ParamTextLazy name text) = name =: text
    urlencoded (ParamNum name number) = name =: number
    urlencoded (ParamJson name value) = name =: encodeToLazyText value
    urlencoded (ParamBytes name payload) = undefined
    multipart (ParamText name text) = Multipart.partBS name $ Encoding.encodeUtf8 text
    multipart (ParamTextLazy name text) = Multipart.partLBS name $ EncodingLazy.encodeUtf8 text
    multipart (ParamNum name number) = Multipart.partLBS name $ BSLC.pack $ show number
    multipart (ParamJson name value) = Multipart.partLBS name $ encode value
    multipart (ParamBytes name payload) = Multipart.partLBS name payload
    escapeText text = Text.replace "\"" "\\\"" $ Text.replace "\\" "\\\\" $ text
    escapeTextLazy text = TextLazy.replace "\"" "\\\"" $ TextLazy.replace "\\" "\\\\" $ text


webDownload :: Logger.Handle -> Address -> IO BSL.ByteString
webDownload logger address = do
    Logger.debug logger $
        "WebDriver: Download a file from " <> address
    (url, options) <- parseUrlVerbose logger address
    resp <- runReq defaultHttpConfig $ req
        GET
        url
        NoReqBody
        lbsResponse
        options
    Logger.debug logger $
        "WebDriver: Response received"
    return $ responseBody resp


parseUrlVerbose :: Logger.Handle -> Text.Text -> IO (Url 'Https, Option scheme)
parseUrlVerbose logger source = do
    case URI.mkURI source >>= useHttpsURI of
        Nothing -> do
            Logger.err logger $
                "WebDriver: Invalid URI: " <> Text.pack (show source)
            fail $ "invalid URL"
        Just result -> return $ result


encodeToText :: Value -> Text.Text
encodeToText = TextLazy.toStrict . encodeToLazyText
