{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module WebDriver
    ( Address(..)
    , Handle(..)
    , withWebDriver
    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Network.HTTP.Req
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified Logger


data Address
    = HttpsAddress Text.Text [Text.Text]
    deriving (Show, Eq)


data Handle
    = Handle
        { request :: forall a b . (ToJSON a, FromJSON b) => Address -> a -> IO b }


{--}


withWebDriver :: Logger.Handle -> (Handle -> IO r) -> IO r
withWebDriver logger body = do
    body $ Handle
        { request = webRequest logger }


newtype HttpIO t
    = HttpIO
        { runHttpIO :: IO t }
    deriving (Functor, Applicative, Monad, MonadIO)


instance MonadHttp HttpIO where
    handleHttpException = HttpIO . throwIO


webRequest :: (ToJSON a, FromJSON b) => Logger.Handle -> Address -> a -> IO b
webRequest logger (HttpsAddress root nodes) params = do
    Logger.info logger $ "Send request to " <> root
    Logger.debug logger $
        "\t" <> Text.pack (show (toJSON params))
    resp <- runHttpIO $ req
        POST
        (foldl (/:) (https root) nodes)
        (ReqBodyJson $ params)
        jsonResponse
        mempty
    let value = responseBody resp :: Value
    Logger.info logger $
        "Response received"
    Logger.debug logger $
        "\t" <> Text.pack (show value)
    case fromJSON value of
        Success x -> do
            Logger.debug logger $ "Response successfully parsed"
            return $ x
        Error e -> do
            Logger.err logger $ "Response failed to parse: " <> Text.pack e
            throwIO $ JsonHttpException e
