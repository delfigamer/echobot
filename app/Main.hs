module Main where


import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Yaml
import GHC.IO.Encoding (textEncodingName)
import System.Environment
import System.IO
import qualified Data.Text as Text
import qualified System.Random as Random
import qualified Channel
import qualified Channel.Tg
import qualified Channel.Vk
import qualified Logger
import qualified Responder
import qualified Responder.Repeat
import qualified WebDriver


main :: IO ()
main = do
    Just encoding1 <- hGetEncoding stdout
    encoding2 <- mkTextEncoding $ textEncodingName encoding1 ++ "//TRANSLIT"
    hSetEncoding stdout encoding2
    args <- getArgs
    ac <- case args of
        [] -> decodeFileThrow "config.yaml"
        arg1:_ -> decodeFileThrow arg1
    withLogger (acLogger ac) $ \logger -> do
        let driverlogger = Logger.loggerFilter (logLevel $ acWebDriverLogLevel ac) logger
        WebDriver.withWebDriver driverlogger $ \driver -> do
            let channellogger = Logger.loggerFilter (logLevel $ acChannelLogLevel ac) logger
            withChannel (acChannel ac) channellogger driver $ \channel -> do
                let responderlogger = Logger.loggerFilter (logLevel $ acResponderLogLevel ac) logger
                withResponder (acResponder ac) responderlogger channel $ \responder -> do
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
withChannel (VkChannelConfig conf) logger driver body = do
    randomSeed <- Random.randomIO
    Channel.Vk.withVkChannel conf randomSeed logger driver body


withLogger :: LoggerConfig -> (Logger.Handle -> IO r) -> IO r
withLogger NullLoggerConfig body = Logger.withNullLogger body
withLogger StdLoggerConfig body = Logger.withStdLogger body
withLogger (FileLoggerConfig filepath) body = Logger.withFileLogger filepath body
withLogger (MultiLoggerConfig logs) body0 = do
    withMulti logs body0
    where
    withMulti [] body = Logger.withNullLogger body
    withMulti [a] body = withLogger a body
    withMulti (a:rest) body = do
        withLogger a $ \la -> do
            withMulti rest $ \lrest -> do
                Logger.withMultiLogger la lrest body
withLogger (FilterLoggerConfig (LogLevelConfig level) logconf) body = do
    withLogger logconf $ \la -> do
        body $ Logger.loggerFilter level la


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
    | VkChannelConfig Channel.Vk.Config
    deriving (Show)


data LoggerConfig
    = NullLoggerConfig
    | StdLoggerConfig
    | FileLoggerConfig FilePath
    | MultiLoggerConfig [LoggerConfig]
    | FilterLoggerConfig LogLevelConfig LoggerConfig
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
        typename <- v .: "type"
        case typename of
            "tg" -> parseTg v
            "vk" -> parseVk v
            _ -> fail $ "unknown channel type: " ++ typename
        where
        parseTg v = do
            (TgChannelConfig <$>) $
                Channel.Tg.Config
                    <$> v .: "tg-token"
                    <*> v .:? "timeout" .!= 60
                    <*> v .:? "keyboard-width" .!= 5
        parseVk v = do
            (VkChannelConfig <$>) $
                Channel.Vk.Config
                    <$> v .: "vk-token"
                    <*> v .: "vk-group-id"
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
                , (,) parseFilter <$> v .: "filter"
                ]
            parser obj
        parseFile = withText "FileLoggerConfig" $ \path -> do
            return $ FileLoggerConfig (Text.unpack path)
        parseFilter = withObject "FilterLoggerConfig" $ \v -> do
            level <- v .:? "level" .!= LogLevelConfig Logger.Debug
            target <- v .: "of"
            return $ FilterLoggerConfig level target


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
                    <*> v .:? "start-cmd" .!= "/start"
                    <*> v .:? "start-msg" .!= "[responder.repeat.start-msg] %1"
                    <*> v .:? "describe-cmd" .!= "/help"
                    <*> v .:? "describe-msg" .!= "[responder.repeat.describe-msg] %1"
                    <*> v .:? "inspect-multiplier-cmd" .!= "/repeat"
                    <*> v .:? "inspect-multiplier-msg" .!= "[responder.repeat.inspect-multiplier-msg] %1"
                    <*> v .:? "multiplier-set-msg" .!= "[responder.repeat.multiplier-set-msg] %1"
                    <*> v .:? "media-unknown-type-msg" .!= "[responder.repeat.media-unknown-type-msg] %1"
                    <*> v .:? "media-unsupported-msg" .!= "[responder.repeat.media-unsupported-msg]"
                    <*> v .:? "media-internal-error-msg" .!= "[responder.repeat.media-internal-error-msg]"
                    <*> v .:? "max-multiplier" .!= 5


instance FromJSON LogLevelConfig where
    parseJSON (String "debug") = return $ LogLevelConfig Logger.Debug
    parseJSON (String "info") = return $ LogLevelConfig Logger.Info
    parseJSON (String "warn") = return $ LogLevelConfig Logger.Warn
    parseJSON (String "err") = return $ LogLevelConfig Logger.Err
    parseJSON (String "none") = return $ LogLevelConfig Logger.Topmost
    parseJSON _ = fail $ "invalid log level"
