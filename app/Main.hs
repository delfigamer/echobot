{-# LANGUAGE OverloadedStrings #-}


module Main where


import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Yaml
import GHC.IO.Encoding (textEncodingName)
import System.IO
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Channel
import qualified Channel.Tg
import qualified Logger
import qualified Responder
import qualified Responder.Repeat
import qualified WebDriver


main :: IO ()
main = do
    Just encoding1 <- hGetEncoding stdout
    encoding2 <- mkTextEncoding $ textEncodingName encoding1 ++ "//TRANSLIT"
    hSetEncoding stdout encoding2
    ac <- decodeFileThrow "config.yaml"
    withLogger (acLogger ac) $ \logger -> do
        WebDriver.withWebDriver logger $ \driver -> do
            withChannel (acChannel ac) logger driver $ \channel -> do
                withResponder (acResponder ac) logger channel $ \responder -> do
                    catch
                        (forever $ Responder.work responder)
                        (logFatal logger)
    where
    logFatal :: Logger.Handle -> SomeException -> IO ()
    logFatal logger err = do
        Logger.err logger $
            "Fatal error: " <> Text.pack (show err)


withChannel :: ChannelConfig -> Logger.Handle -> WebDriver.Handle -> (Channel.Handle -> IO r) -> IO r
withChannel (TgChannelConfig conf) logger driver body = do
    Channel.Tg.withTgChannel conf logger driver body


withLogger :: LoggerConfig -> (Logger.Handle -> IO r) -> IO r
withLogger NullLoggerConfig body = Logger.withNullLogger body
withLogger StdLoggerConfig body = Logger.withStdLogger body
withLogger (FileLoggerConfig filepath) body = Logger.withFileLogger filepath body
withLogger (MultiLoggerConfig logs) body = do
    withMulti logs body
    where
    withMulti [] body = Logger.withNullLogger body
    withMulti [a] body = withLogger a body
    withMulti (a:rest) body = do
        withLogger a $ \la -> do
            withMulti rest $ \lrest -> do
                Logger.withMultiLogger la lrest body


withResponder :: ResponderConfig -> Logger.Handle -> Channel.Handle -> (Responder.Handle -> IO r) -> IO r
withResponder (RepeatResponderConfig conf) logger channel body = do
    Responder.Repeat.withRepeatResponder conf logger channel body


data AppConfig
    = AppConfig
        { acChannel :: ChannelConfig
        , acLogger :: LoggerConfig
        , acResponder :: ResponderConfig
        , acWebDriverLogLevel :: LogLevelConfig
        , acChannelLogLevel :: LogLevelConfig
        , acResponderLogLevel :: LogLevelConfig }
    deriving (Show)


data ChannelConfig
    = TgChannelConfig Channel.Tg.Config
    deriving (Show)


data LoggerConfig
    = NullLoggerConfig
    | StdLoggerConfig
    | FileLoggerConfig FilePath
    | MultiLoggerConfig [LoggerConfig]
    deriving (Show)


data ResponderConfig
    = RepeatResponderConfig Responder.Repeat.Config
    deriving (Show)


newtype LogLevelConfig
    = LogLevelConfig { logLevel :: Logger.LogLevel }
    deriving (Show)


instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \v -> do
        AppConfig
            <$> v .: "channel"
            <*> v .:? "logger" .!= StdLoggerConfig
            <*> v .: "responder"
            <*> v .:? "web-log-level" .!= LogLevelConfig Logger.Debug
            <*> v .:? "channel-log-level" .!= LogLevelConfig Logger.Debug
            <*> v .:? "responder-log-level" .!= LogLevelConfig Logger.Debug


instance FromJSON ChannelConfig where
    parseJSON = withObject "ChannelConfig" $ \v -> do
        (parser, obj) <- msum
            [ (,) parseTg <$> v .: "tg"
            ]
        parser obj
        where
        parseTg = withObject "TgChannelConfig" $ \v -> do
            (TgChannelConfig <$>) $
                Channel.Tg.Config
                    <$> v .: "token"
                    <*> v .:? "timeout" .!= 60
                    <*> v .:? "keyboard-width" .!= 5


instance FromJSON LoggerConfig where
    parseJSON x = asText x `mplus` asArray x `mplus` asObject x
        where
        asText = withText "LoggerConfig" $ \text -> do
            msum
                [ guard (text == "null") >> return NullLoggerConfig
                , guard (text == "std") >> return StdLoggerConfig
                ]
        asArray = withArray "LoggerConfig" $ \xs -> do
            MultiLoggerConfig . toList <$> mapM parseJSON xs
        asObject = withObject "LoggerConfig" $ \v -> do
            (parser, obj) <- msum
                [ (,) parseFile <$> v .: "file"
                ]
            parser obj
        parseFile = withText "FileLoggerConfig" $ \path -> do
            return $ FileLoggerConfig (Text.unpack path)


instance FromJSON ResponderConfig where
    parseJSON = withObject "ResponderConfig" $ \v -> do
        (parser, obj) <- msum
            [ (,) parseRepeat <$> v .: "repeat"
            ]
        parser obj
        where
        parseRepeat = withObject "RepeatResponderConfig" $ \v -> do
            (RepeatResponderConfig <$>) $
                Responder.Repeat.Config
                    <$> v .:? "default-multiplier" .!= 2
                    <*> v .:? "unknown-command-msg" .!= "[responder.repeat.unknown-command-msg]"
                    <*> v .:? "describe-cmd" .!= "/help"
                    <*> v .:? "describe-msg" .!= "[responder.repeat.describe-msg]"
                    <*> v .:? "inspect-multiplier-cmd" .!= "/repeat"
                    <*> v .:? "inspect-multiplier-msg" .!= "[responder.repeat.inspect-multiplier-msg] %1"
                    <*> v .:? "multiplier-set-msg" .!= "[responder.repeat.multiplier-set-msg] %1"
                    <*> v .:? "max-multiplier" .!= 5


instance FromJSON LogLevelConfig where
    parseJSON (String "debug") = return $ LogLevelConfig Logger.Debug
    parseJSON (String "info") = return $ LogLevelConfig Logger.Info
    parseJSON (String "warn") = return $ LogLevelConfig Logger.Warn
    parseJSON (String "err") = return $ LogLevelConfig Logger.Err
    parseJSON (String "none") = return $ LogLevelConfig Logger.Topmost
    parseJSON _ = fail $ "invalid log level"
