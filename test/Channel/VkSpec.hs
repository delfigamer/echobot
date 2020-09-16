{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel.VkSpec
    (
    -- spec
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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TextEncoding
import qualified System.IO as IO
import qualified Channel
import qualified Channel.Vk.Internal as Vk
import qualified Logger
import qualified WebDriver


data ExpectedRequest
    = ExpectedRequest !WebDriver.Address !Value !Value
    | ExpectedDownload !WebDriver.Address ![(Text, Text)] !BSL.ByteString
    | ExpectedUpload !WebDriver.Address !BSL.ByteString !Value
    deriving (Show)


withTestDriver
    :: (IORef [ExpectedRequest] -> WebDriver.Handle -> IO r)
    -> IO r
withTestDriver body = do
    pbuf <- newIORef []
    body pbuf $ WebDriver.Handle
            { WebDriver.request = testDriverRequest pbuf
            , WebDriver.download = testDriverDownload pbuf
            , WebDriver.upload = testDriverUpload pbuf
            }


testDriverRequest
    :: (ToJSON a, FromJSON b)
    => IORef [ExpectedRequest] -> WebDriver.Address -> a -> IO b
testDriverRequest pbuf address params = do
    ExpectedRequest address2 jparams2 result:rest <- readIORef pbuf
    (address, toJSON params) `shouldBe` (address2, jparams2)
    writeIORef pbuf $! rest
    case fromJSON result of
        Error err -> fail err
        Success x -> return $ x


testDriverDownload
    :: IORef [ExpectedRequest] -> WebDriver.Address -> [(Text, Text)] -> IO BSL.ByteString
testDriverDownload pbuf address qparams = do
    ExpectedDownload address2 qparams2 result:rest <- readIORef pbuf
    (address, qparams) `shouldBe` (address2, qparams2)
    writeIORef pbuf $! rest
    return $ result


testDriverUpload
    :: (FromJSON b)
    => IORef [ExpectedRequest] -> WebDriver.Address -> BSL.ByteString -> IO b
testDriverUpload pbuf address content = do
    ExpectedUpload address2 content2 result:rest <- readIORef pbuf
    (address, content) `shouldBe` (address2, content2)
    writeIORef pbuf $! rest
    case fromJSON result of
        Error err -> fail err
        Success x -> return $ x


perform
    :: (Show r, Eq r)
    => IORef [ExpectedRequest]
    -> IO r
    -> [ExpectedRequest]
    -> r
    -> IO ()
perform pbuf act reqs expret = do
    writeIORef pbuf $! reqs
    ret <- act
    bufrest <- readIORef pbuf
    bufrest `shouldSatisfy` null
    ret `shouldBe` expret


spec :: Spec
spec = do
    describe "Channel.Vk" $ do
        let token = "bottok"
        let groupId = "groupId"
        let timeout = 56
        let conf = Vk.Config
                { Vk.cToken = token
                , Vk.cGroupId = groupId
                , Vk.cTimeout = timeout
                , Vk.cKeyboardWidth = 3 }
        let groupChatId x = 2000000000 + x
        it "performs a long poll" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                (WebDriver.HttpsAddress "api.vk.com" ["method", "groups.getLongPollServer"])
                                (object
                                    [ "v" .= Vk.apiVersion
                                    , "access_token" .= token
                                    , "group_id" .= groupId
                                    ])
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "1"
                                        ]
                                    ])
                            , ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey"
                                    , "ts" .= String "1"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "ts" .= String "1"
                                    , "updates" .= Array mempty
                                    ])
                            ]
                            []
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey"
                                    , "ts" .= String "1"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "ts" .= String "1"
                                    , "updates" .= Array mempty
                                    ])
                            ]
                            []
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey"
                                    , "ts" .= String "1"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "ts" .= String "20"
                                    , "failed" .= Number 1
                                    ])
                            ]
                            []
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey"
                                    , "ts" .= String "20"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "failed" .= Number 2
                                    ])
                            , ExpectedRequest
                                (WebDriver.HttpsAddress "api.vk.com" ["method", "groups.getLongPollServer"])
                                (object
                                    [ "v" .= Vk.apiVersion
                                    , "access_token" .= token
                                    , "group_id" .= groupId
                                    ])
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey2"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "30"
                                        ]
                                    ])
                            , ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey2"
                                    , "ts" .= String "30"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "ts" .= String "30"
                                    , "updates" .= Array mempty
                                    ])
                            ]
                            []
        it "receives plain text messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                (WebDriver.HttpsAddress "api.vk.com" ["method", "groups.getLongPollServer"])
                                (object
                                    [ "v" .= Vk.apiVersion
                                    , "access_token" .= token
                                    , "group_id" .= groupId
                                    ])
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "10"
                                        ]
                                    ])
                            , ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey"
                                    , "ts" .= String "10"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "ts" .= String "20"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "text" .= String "100 message text"
                                                    ]
                                                ]
                                            ]
                                        , object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 200
                                                    , "conversation_message_id" .= Number 11
                                                    , "text" .= String "200 message text"
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            [ Channel.EventMessage 100 10 $ Channel.plainText "100 message text"
                            , Channel.EventMessage 200 11 $ Channel.plainText "200 message text"
                            ]
        it "receives media messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                (WebDriver.HttpsAddress "api.vk.com" ["method", "groups.getLongPollServer"])
                                (object
                                    [ "v" .= Vk.apiVersion
                                    , "access_token" .= token
                                    , "group_id" .= groupId
                                    ])
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "10"
                                        ]
                                    ])
                            , ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey"
                                    , "ts" .= String "10"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "ts" .= String "20"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "text" .= String "100 message text"
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "photo"
                                                            , "photo" .= object
                                                                [ "owner_id" .= Number 50
                                                                , "id" .= Number 60
                                                                , "sizes" .=
                                                                    [ object ["width" .= Number 100, "height" .= Number 50, "url" .= String "https://srv.userapi.com/group/file1.jpg"]
                                                                    , object ["width" .= Number 100, "height" .= Number 70, "url" .= String "https://srv.userapi.com/group/file2.jpg"]
                                                                    ]
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "photo"
                                                            , "photo" .= object
                                                                [ "owner_id" .= Number 70
                                                                , "id" .= Number 80
                                                                , "access_key" .= String "beef"
                                                                , "sizes" .=
                                                                    [ object ["width" .= Number 100, "height" .= Number 200, "url" .= String "https://srv.userapi.com/group/file3.jpg"]
                                                                    , object ["width" .= Number 150, "height" .= Number 200, "url" .= String "https://srv.userapi.com/group/file4.jpg"]
                                                                    ]
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        , object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 200
                                                    , "conversation_message_id" .= Number 11
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "photo"
                                                            , "photo" .= object
                                                                [ "owner_id" .= Number 15
                                                                , "id" .= Number 20
                                                                , "access_key" .= String "abab"
                                                                , "sizes" .=
                                                                    [ object ["width" .= Number 100, "height" .= Number 100, "url" .= String "https://srv.userapi.com/group/file5.jpg"]
                                                                    ]
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "photo"
                                                            , "photo" .= object
                                                                [ "owner_id" .= Number 16
                                                                , "id" .= Number 25
                                                                , "access_key" .= String "cdcd"
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        , object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number (groupChatId 300)
                                                    , "conversation_message_id" .= Number 12
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "photo"
                                                            , "photo" .= object
                                                                [ "owner_id" .= Number 17
                                                                , "id" .= Number 30
                                                                , "sizes" .=
                                                                    [ object ["width" .= Number 100, "height" .= Number 100, "url" .= String "https://srv.userapi.com/group/file6.jpg"]
                                                                    ]
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "photo"
                                                            , "photo" .= object
                                                                [ "owner_id" .= Number 18
                                                                , "id" .= Number 40
                                                                , "access_key" .= String "efef"
                                                                , "sizes" .=
                                                                    [ object ["width" .= Number 100, "height" .= Number 100, "url" .= String "https://srv.userapi.com/group/file7.jpg"]
                                                                    ]
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            [ Channel.EventMedia 100 "100 message text"
                                [ Channel.ForeignMedia Channel.MediaPhoto "photo50_60" "https://srv.userapi.com/group/file2.jpg"
                                , Channel.ForeignMedia Channel.MediaPhoto "photo70_80_beef" "https://srv.userapi.com/group/file4.jpg"
                                ]
                            , Channel.EventMedia 200 ""
                                [ Channel.ForeignMedia Channel.MediaPhoto "photo15_20_abab" "https://srv.userapi.com/group/file5.jpg"
                                , Channel.ForeignMedia Channel.MediaPhoto "photo16_25_cdcd" ""
                                ]
                            , Channel.EventMedia (groupChatId 300) ""
                                [ Channel.ForeignMedia Channel.MediaPhoto "photo17_30" "https://srv.userapi.com/group/file6.jpg"
                                , Channel.ForeignMedia Channel.MediaPhoto "" "https://srv.userapi.com/group/file7.jpg"
                                ]
                            ]
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                (WebDriver.HttpsAddress "lp.vk.com" ["lpadr", "lpadr2"])
                                (object
                                    [ "key" .= String "lpkey"
                                    , "ts" .= String "20"
                                    , "act" .= String  "a_check"
                                    , "wait" .= timeout
                                    ])
                                (object
                                    [ "ts" .= String "30"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "text" .= String "100 message text"
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "video"
                                                            , "video" .= object
                                                                [ "owner_id" .= Number 50
                                                                , "id" .= Number 60
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "video"
                                                            , "video" .= object
                                                                [ "owner_id" .= Number 70
                                                                , "id" .= Number 80
                                                                , "access_key" .= String "beef"
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        , object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number (groupChatId 300)
                                                    , "conversation_message_id" .= Number 12
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "video"
                                                            , "video" .= object
                                                                [ "owner_id" .= Number 17
                                                                , "id" .= Number 30
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "video"
                                                            , "video" .= object
                                                                [ "owner_id" .= Number 18
                                                                , "id" .= Number 40
                                                                , "access_key" .= String "efef"
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            [ Channel.EventMedia 100 "100 message text"
                                [ Channel.ForeignMedia Channel.MediaVideo "video50_60" ""
                                , Channel.ForeignMedia Channel.MediaVideo "video70_80_beef" ""
                                ]
                            , Channel.EventMedia (groupChatId 300) ""
                                [ Channel.ForeignMedia Channel.MediaVideo "video17_30" ""
                                ]
                            ]
                        {- attachents with an access_key that come from group chats are busted, see Channel.Vk.Internal for a more detailed explanation -}
        -- it "sends text messages" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "text" .= ("message" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= object ["message_id" .= (600 :: Int)]
                                -- ])
                            -- (Channel.sendMessage channel 100 (Channel.plainText "message") [])
                            -- (Right 600)
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (102 :: Int)
                                -- , "text" .= ("message 2" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= False
                                -- , "description" .= ("err" :: Text)
                                -- ])
                            -- (Channel.sendMessage channel 102 (Channel.plainText "message 2") [])
                            -- (Left "err")
        -- it "sends sticker messages" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendSticker"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "sticker" .= ("sticker" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendSticker channel 100 "sticker")
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendSticker"])
                            -- (object
                                -- [ "chat_id" .= (102 :: Int)
                                -- , "sticker" .= ("sticker 2" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= False
                                -- , "description" .= ("err" :: Text)
                                -- ])
                            -- (Channel.sendSticker channel 102 "sticker 2")
                            -- (Left "err")
        -- it "updates existing messages" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "editMessageText"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "message_id" .= (600 :: Int)
                                -- , "text" .= ("updated message" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.updateMessage channel 100 600 (Channel.plainText "updated message") [])
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "editMessageText"])
                            -- (object
                                -- [ "chat_id" .= (102 :: Int)
                                -- , "message_id" .= (700 :: Int)
                                -- , "text" .= ("updated message 2" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= False
                                -- , "description" .= ("err" :: Text)
                                -- ])
                            -- (Channel.updateMessage channel 102 700 (Channel.plainText "updated message 2") [])
                            -- (Left "err")
        -- it "sends messages with buttons" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequestWithReplyMarkup phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "text" .= ("message" :: Text)
                                -- ])
                            -- (object
                                -- [ "inline_keyboard" .=
                                    -- [ [ object ["text" .= ("t1" :: Text), "callback_data" .= ("u1" :: Text)]
                                      -- , object ["text" .= ("t2" :: Text), "callback_data" .= ("u2" :: Text)]
                                      -- , object ["text" .= ("t3" :: Text), "callback_data" .= ("u3" :: Text)]
                                      -- ]
                                    -- , [ object ["text" .= ("t4" :: Text), "callback_data" .= ("u4" :: Text)]
                                      -- ]
                                    -- ]
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= object ["message_id" .= (600 :: Int)]
                                -- ])
                            -- (Channel.sendMessage channel 100 (Channel.plainText "message")
                                -- [ Channel.QueryButton "t1" "u1"
                                -- , Channel.QueryButton "t2" "u2"
                                -- , Channel.QueryButton "t3" "u3"
                                -- , Channel.QueryButton "t4" "u4"
                                -- ])
                            -- (Right 600)
        -- it "updates messages with buttons" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequestWithReplyMarkup phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "editMessageText"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "message_id" .= (600 :: Int)
                                -- , "text" .= ("updated message" :: Text)
                                -- ])
                            -- (object
                                -- [ "inline_keyboard" .=
                                    -- [ [ object ["text" .= ("t1" :: Text), "callback_data" .= ("u1" :: Text)]
                                      -- , object ["text" .= ("t2" :: Text), "callback_data" .= ("u2" :: Text)]
                                      -- , object ["text" .= ("t3" :: Text), "callback_data" .= ("u3" :: Text)]
                                      -- ]
                                    -- , [ object ["text" .= ("t4" :: Text), "callback_data" .= ("u4" :: Text)]
                                      -- , object ["text" .= ("t5" :: Text), "callback_data" .= ("u5" :: Text)]
                                      -- ]
                                    -- ]
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.updateMessage channel 100 600 (Channel.plainText "updated message")
                                -- [ Channel.QueryButton "t1" "u1"
                                -- , Channel.QueryButton "t2" "u2"
                                -- , Channel.QueryButton "t3" "u3"
                                -- , Channel.QueryButton "t4" "u4"
                                -- , Channel.QueryButton "t5" "u5"
                                -- ])
                            -- (Right ())
        -- it "receives button events" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (1 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "message_id" .= (500 :: Int)
                                            -- , "text" .= ("Sample Text" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (2 :: Int)
                                        -- , "callback_query" .= object
                                            -- [ "message" .= object
                                                -- [ "chat" .= object ["id" .= (101 :: Int)]
                                                -- , "message_id" .= (501 :: Int)
                                                -- ]
                                            -- , "id" .= ("qid1" :: Text)
                                            -- , "data" .= ("qdata1" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (3 :: Int)
                                        -- , "callback_query" .= object
                                            -- [ "message" .= object
                                                -- [ "chat" .= object ["id" .= (102 :: Int)]
                                                -- , "message_id" .= (502 :: Int)
                                                -- ]
                                            -- , "id" .= ("qid2" :: Text)
                                            -- , "data" .= ("qdata2" :: Text)
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMessage 100 500 (Channel.plainText "Sample Text")
                            -- , Channel.EventQuery 101 501 "qid1" "qdata1"
                            -- , Channel.EventQuery 102 502 "qid2" "qdata2"
                            -- ]
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["offset" .= (4 :: Int), "timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (4 :: Int)
                                        -- , "callback_query" .= object
                                            -- [ "message" .= object
                                                -- [ "chat" .= object ["id" .= (110 :: Int)]
                                                -- , "message_id" .= (510 :: Int)
                                                -- ]
                                            -- , "id" .= ("qid5" :: Text)
                                            -- , "data" .= ("qdata5" :: Text)
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventQuery 110 510 "qid5" "qdata5"
                            -- ]
        -- it "sends answers to button queries" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "answerCallbackQuery"])
                            -- (object
                                -- [ "callback_query_id" .= ("qid1" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.answerQuery channel "qid1" "")
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "answerCallbackQuery"])
                            -- (object
                                -- [ "callback_query_id" .= ("qid2" :: Text)
                                -- , "text" .= ("sample text" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.answerQuery channel "qid2" "sample text")
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "answerCallbackQuery"])
                            -- (object
                                -- [ "callback_query_id" .= ("qid3" :: Text)
                                -- , "text" .= ("sample text 3" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= False
                                -- , "description" .= ("err" :: Text)
                                -- ])
                            -- (Channel.answerQuery channel "qid3" "sample text 3")
                            -- (Left "err")
        -- it "receives media messages" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (1 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "photo" .=
                                                -- [ object ["file_id" .= ("photo 1" :: Text)]
                                                -- ]
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (2 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "photo" .=
                                                -- [ object ["file_id" .= ("photo 2" :: Text)]
                                                -- ]
                                            -- , "caption" .= ("caption 2" :: Text)
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMedia 100 "" (Channel.MediaPhoto "photo 1")
                            -- , Channel.EventMedia 100 "caption 2" (Channel.MediaPhoto "photo 2")
                            -- ]
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["offset" .= (3 :: Int), "timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (3 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "video" .= object ["file_id" .= ("video id" :: Text)]
                                            -- , "caption" .= ("video caption" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (4 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "audio" .= object ["file_id" .= ("audio id" :: Text)]
                                            -- , "caption" .= ("audio caption" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (5 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "animation" .= object ["file_id" .= ("animation id" :: Text)]
                                            -- , "document" .= object ["file_id" .= ("other id" :: Text)]
                                            -- , "caption" .= ("animation caption" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (6 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "voice" .= object ["file_id" .= ("voice id" :: Text)]
                                            -- , "caption" .= ("voice caption" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (7 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "document" .= object ["file_id" .= ("document id" :: Text)]
                                            -- , "caption" .= ("document caption" :: Text)
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMedia 100 "video caption" $ Channel.MediaVideo "video id"
                            -- , Channel.EventMedia 100 "audio caption" $ Channel.MediaAudio "audio id"
                            -- , Channel.EventMedia 100 "animation caption" $ Channel.MediaAnimation "animation id"
                            -- , Channel.EventMedia 100 "voice caption" $ Channel.MediaVoice "voice id"
                            -- , Channel.EventMedia 100 "document caption" $ Channel.MediaDocument "document id"
                            -- ]
        -- it "receives media group messages" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (1 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "photo" .=
                                                -- [ object ["file_id" .= ("photo 1" :: Text)]
                                                -- ]
                                            -- , "caption" .= ("caption 1" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (2 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "photo" .=
                                                -- [ object ["file_id" .= ("photo 2" :: Text)]
                                                -- ]
                                            -- , "caption" .= ("caption 2" :: Text)
                                            -- , "media_group_id" .= ("group id" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (3 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "photo" .=
                                                -- [ object ["file_id" .= ("photo 3" :: Text)]
                                                -- ]
                                            -- , "media_group_id" .= ("group id" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (4 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "video" .= object ["file_id" .= ("video 4" :: Text)]
                                            -- , "caption" .= ("caption 4" :: Text)
                                            -- , "media_group_id" .= ("group id" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (5 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "video" .= object ["file_id" .= ("video 5" :: Text)]
                                            -- , "caption" .= ("caption 5" :: Text)
                                            -- , "media_group_id" .= ("group id 2" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (6 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "video" .= object ["file_id" .= ("video 6" :: Text)]
                                            -- , "caption" .= ("caption 6" :: Text)
                                            -- , "media_group_id" .= ("group id 2" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (7 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (200 :: Int)]
                                            -- , "video" .= object ["file_id" .= ("video 7" :: Text)]
                                            -- , "caption" .= ("caption 7" :: Text)
                                            -- , "media_group_id" .= ("group id 2" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (8 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (200 :: Int)]
                                            -- , "video" .= object ["file_id" .= ("video 8" :: Text)]
                                            -- , "caption" .= ("caption 8" :: Text)
                                            -- , "media_group_id" .= ("group id 2" :: Text)
                                            -- ]
                                        -- ]
                                    -- , object
                                        -- [ "update_id" .= (5 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "photo" .=
                                                -- [ object ["file_id" .= ("photo 9" :: Text)]
                                                -- ]
                                            -- , "caption" .= ("caption 9" :: Text)
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMedia 100 "caption 1" $ Channel.MediaPhoto "photo 1"
                            -- , Channel.EventMediaGroup 100 "group id"
                                -- $ Channel.MediaGroupPhoto "caption 2" "photo 2"
                                -- $ Channel.MediaGroupPhoto "" "photo 3"
                                -- $ Channel.MediaGroupVideo "caption 4" "video 4"
                                -- $ Channel.MediaGroupEnd
                            -- , Channel.EventMediaGroup 100 "group id 2"
                                -- $ Channel.MediaGroupVideo "caption 5" "video 5"
                                -- $ Channel.MediaGroupVideo "caption 6" "video 6"
                                -- $ Channel.MediaGroupEnd
                            -- , Channel.EventMediaGroup 200 "group id 2"
                                -- $ Channel.MediaGroupVideo "caption 7" "video 7"
                                -- $ Channel.MediaGroupVideo "caption 8" "video 8"
                                -- $ Channel.MediaGroupEnd
                            -- , Channel.EventMedia 100 "caption 9" $ Channel.MediaPhoto "photo 9"
                            -- ]
        -- it "sends media" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendPhoto"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "photo" .= ("photo 1" :: Text)
                                -- , "caption" .= ("caption 1" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMedia channel 100 "caption 1" (Channel.MediaPhoto "photo 1"))
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendVideo"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "video" .= ("video 2" :: Text)
                                -- , "caption" .= ("caption 2" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMedia channel 100 "caption 2" (Channel.MediaVideo "video 2"))
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendAudio"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "audio" .= ("audio 3" :: Text)
                                -- , "caption" .= ("caption 3" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMedia channel 100 "caption 3" (Channel.MediaAudio "audio 3"))
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendAnimation"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "animation" .= ("animation 4" :: Text)
                                -- , "caption" .= ("caption 4" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMedia channel 100 "caption 4" (Channel.MediaAnimation "animation 4"))
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendVoice"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "voice" .= ("voice 5" :: Text)
                                -- , "caption" .= ("caption 5" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMedia channel 100 "caption 5" (Channel.MediaVoice "voice 5"))
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendDocument"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "document" .= ("document 6" :: Text)
                                -- , "caption" .= ("caption 6" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMedia channel 100 "caption 6" (Channel.MediaDocument "document 6"))
                            -- (Right ())
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendPhoto"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "photo" .= ("photo 7" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMedia channel 100 "" (Channel.MediaPhoto "photo 7"))
                            -- (Right ())
        -- it "sends media groups" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMediaGroup"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "media" .=
                                    -- [ object
                                        -- [ "type" .= ("photo" :: Text)
                                        -- , "media" .= ("photo 1" :: Text)
                                        -- ]
                                    -- , object
                                        -- [ "type" .= ("photo" :: Text)
                                        -- , "media" .= ("photo 2" :: Text)
                                        -- , "caption" .= ("caption 2" :: Text)
                                        -- ]
                                    -- , object
                                        -- [ "type" .= ("video" :: Text)
                                        -- , "media" .= ("video 3" :: Text)
                                        -- , "caption" .= ("caption 3" :: Text)
                                        -- ]
                                    -- ]
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= True
                                -- ])
                            -- (Channel.sendMediaGroup channel 100
                                -- $ Channel.MediaGroupPhoto "" "photo 1"
                                -- $ Channel.MediaGroupPhoto "caption 2" "photo 2"
                                -- $ Channel.MediaGroupVideo "caption 3" "video 3"
                                -- $ Channel.MediaGroupEnd)
                            -- (Right ())
        -- it "receives rich text messages" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (1 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "message_id" .= (500 :: Int)
                                            -- {-                                                            ^23456789_12^23456789_      -}
                                            -- {-                  ^2345^234567^23456789_1      ^23456^23456789_123456789^23456789_12345 -}
                                            -- , "text" .= ("plain bold italic bolditalic plain under strike understrike boldunder under" :: Text)
                                            -- {-            _123456789_123456789_123456789_123456789_123456789_123456789_123456789_1234 -}
                                            -- , "entities" .=
                                                -- [ object ["type" .= ("bold" :: Text), "offset" .= (6 :: Int), "length" .= (5 :: Int)]
                                                -- , object ["type" .= ("italic" :: Text), "offset" .= (11 :: Int), "length" .= (7 :: Int)]
                                                -- , object ["type" .= ("bold" :: Text), "offset" .= (18 :: Int), "length" .= (11 :: Int)]
                                                -- , object ["type" .= ("italic" :: Text), "offset" .= (18 :: Int), "length" .= (11 :: Int)]
                                                -- , object ["type" .= ("underline" :: Text), "offset" .= (35 :: Int), "length" .= (6 :: Int)]
                                                -- , object ["type" .= ("strikethrough" :: Text), "offset" .= (41 :: Int), "length" .= (19 :: Int)]
                                                -- , object ["type" .= ("underline" :: Text), "offset" .= (48 :: Int), "length" .= (12 :: Int)]
                                                -- , object ["type" .= ("underline" :: Text), "offset" .= (60 :: Int), "length" .= (15 :: Int)]
                                                -- , object ["type" .= ("bold" :: Text), "offset" .= (60 :: Int), "length" .= (10 :: Int)]
                                                -- ]
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMessage 100 500
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False True False False) "italic "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True True False False) "bolditalic "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False True) "strike "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True True) "understrike "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True False True False) "boldunder "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                -- $ Channel.RichTextEnd
                            -- ]
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["offset" .= (2 :: Int), "timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (2 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "message_id" .= (500 :: Int)
                                            -- {-                 ^234                    ^2345 -}
                                            -- {-            ^23456789_1234       ^23456789_123 -}
                                            -- , "text" .= ("link bold link plain mention under" :: Text)
                                            -- {-            _123456789_123456789_123456789_123 -}
                                            -- , "entities" .=
                                                -- [ object ["type" .= ("bold" :: Text), "offset" .= (5 :: Int), "length" .= (4 :: Int)]
                                                -- , object ["type" .= ("underline" :: Text), "offset" .= (29 :: Int), "length" .= (5 :: Int)]
                                                -- , object ["type" .= ("text_link" :: Text), "offset" .= (0 :: Int), "length" .= (14 :: Int), "url" .= ("link url" :: Text)]
                                                -- , object ["type" .= ("text_mention" :: Text), "offset" .= (21 :: Int), "length" .= (13 :: Int), "user" .= object ["id" .= ("user id" :: Text)]]
                                                -- ]
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMessage 100 500
                                -- $ Channel.RichTextLink "link url"
                                    -- ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "link "
                                    -- $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold"
                                    -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " link"
                                    -- $ Channel.RichTextEnd )
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                -- $ Channel.RichTextMention "user id"
                                    -- ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "mention "
                                    -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                    -- $ Channel.RichTextEnd )
                                -- $ Channel.RichTextEnd
                            -- ]
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["offset" .= (3 :: Int), "timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (3 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "message_id" .= (500 :: Int)
                                            -- {-            ^23456789_1       ^23456789_       ^23456789_12345 -}
                                            -- , "text" .= ("inline-code plain block-code plain block-code-lang" :: Text)
                                            -- {-            _123456789_123456789_123456789_123456789_123456789 -}
                                            -- , "entities" .=
                                                -- [ object ["type" .= ("code" :: Text), "offset" .= (0 :: Int), "length" .= (11 :: Int)]
                                                -- , object ["type" .= ("ignore me" :: Text), "offset" .= (14 :: Int), "length" .= (4 :: Int)]
                                                -- , object ["type" .= ("pre" :: Text), "offset" .= (18 :: Int), "length" .= (10 :: Int)]
                                                -- , object ["type" .= ("pre" :: Text), "offset" .= (35 :: Int), "length" .= (15 :: Int), "language" .= ("code lang" :: Text)]
                                                -- ]
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMessage 100 500
                                -- $ Channel.RichTextMono "inline-code"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                -- $ Channel.RichTextCode "" "block-code"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                -- $ Channel.RichTextCode "code lang" "block-code-lang"
                                -- $ Channel.RichTextEnd
                            -- ]
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            -- (object ["offset" .= (4 :: Int), "timeout" .= timeout])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .=
                                    -- [ object
                                        -- [ "update_id" .= (4 :: Int)
                                        -- , "message" .= object
                                            -- [ "chat" .= object ["id" .= (100 :: Int)]
                                            -- , "message_id" .= (500 :: Int)
                                            -- {-                       ^     345                ^2 -}
                                            -- , "text" .= ("tt \x1F914\x1F914 bb \x1F914\x1F914 uu tt" :: Text)
                                            -- {-            _12 3      5     789_ 1      3     567 -}
                                            -- , "entities" .=
                                                -- [ object ["type" .= ("bold" :: Text), "offset" .= (5 :: Int), "length" .= (5 :: Int)]
                                                -- , object ["type" .= ("underline" :: Text), "offset" .= (16 :: Int), "length" .= (2 :: Int)]
                                                -- ]
                                            -- ]
                                        -- ]
                                    -- ]
                                -- ])
                            -- (Channel.poll channel)
                            -- [ Channel.EventMessage 100 500
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "tt \x1F914"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "\x1F914 bb"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " \x1F914\x1F914 "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "uu"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " tt"
                                -- $ Channel.RichTextEnd
                            -- ]
        -- it "sends rich text messages" $ do
            -- Logger.withNullLogger $ \logger -> do
                -- withTestDriver $ \phandler driver -> do
                    -- Tg.withTgChannel conf logger driver $ \channel -> do
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "text" .= ("\
                                    -- \plain \
                                    -- \<b>bold </b>\
                                    -- \<i>italic </i>\
                                    -- \<b><i>bolditalic </i></b>\
                                    -- \plain \
                                    -- \<u>under </u>\
                                    -- \<s>strike </s>\
                                    -- \<u><s>understrike </s></u>\
                                    -- \<b><u>boldunder </u></b>\
                                    -- \<u>under</u>" :: Text)
                                -- , "parse_mode" .= ("HTML" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= object ["message_id" .= (600 :: Int)]
                                -- ])
                            -- (Channel.sendMessage channel 100
                                -- ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False True False False) "italic "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True True False False) "bolditalic "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False True) "strike "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True True) "understrike "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True False True False) "boldunder "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                -- $ Channel.RichTextEnd )
                                -- [])
                            -- (Right 600)
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "text" .= ("\
                                    -- \<a href=\"link url\">link <b>bold</b> link</a>\
                                    -- \ plain \
                                    -- \<a href=\"tg://user?id=user id\">mention <u>under</u></a>" :: Text)
                                -- , "parse_mode" .= ("HTML" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= object ["message_id" .= (601 :: Int)]
                                -- ])
                            -- (Channel.sendMessage channel 100
                                -- ( Channel.RichTextLink "link url"
                                    -- ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "link "
                                    -- $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold"
                                    -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " link"
                                    -- $ Channel.RichTextEnd )
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                -- $ Channel.RichTextMention "user id"
                                    -- ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "mention "
                                    -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                    -- $ Channel.RichTextEnd )
                                -- $ Channel.RichTextEnd )
                                -- [])
                            -- (Right 601)
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "text" .= ("\
                                    -- \<code>inline-code</code>\
                                    -- \ plain \
                                    -- \<pre>block-code</pre>\
                                    -- \ plain \
                                    -- \<pre><code class=\"language-code lang\">block-code-lang</code></pre>" :: Text)
                                -- , "parse_mode" .= ("HTML" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= object ["message_id" .= (602 :: Int)]
                                -- ])
                            -- (Channel.sendMessage channel 100
                                -- ( Channel.RichTextMono "inline-code"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                -- $ Channel.RichTextCode "" "block-code"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                -- $ Channel.RichTextCode "code lang" "block-code-lang"
                                -- $ Channel.RichTextEnd )
                                -- [])
                            -- (Right 602)
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "text" .= ("\
                                    -- \tt \x1F914\
                                    -- \<b>\x1F914 bb</b>\
                                    -- \ \x1F914\x1F914 \
                                    -- \<u>uu</u>\
                                    -- \ tt" :: Text)
                                -- , "parse_mode" .= ("HTML" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= object ["message_id" .= (603 :: Int)]
                                -- ])
                            -- (Channel.sendMessage channel 100
                                -- ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "tt \x1F914"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "\x1F914 bb"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " \x1F914\x1F914 "
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "uu"
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " tt"
                                -- $ Channel.RichTextEnd )
                                -- [])
                            -- (Right 603)
                        -- oneRequest phandler
                            -- (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            -- (object
                                -- [ "chat_id" .= (100 :: Int)
                                -- , "text" .= ("\
                                    -- \abc&lt;&gt;&amp;&quot;\
                                    -- \<b>abc&lt;&gt;&amp;&quot;</b>\
                                    -- \abc&lt;&gt;&amp;&quot;" :: Text)
                                -- , "parse_mode" .= ("HTML" :: Text)
                                -- ])
                            -- (object
                                -- [ "ok" .= True
                                -- , "result" .= object ["message_id" .= (604 :: Int)]
                                -- ])
                            -- (Channel.sendMessage channel 100
                                -- ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "abc<>&\""
                                -- $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "abc<>&\""
                                -- $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "abc<>&\""
                                -- $ Channel.RichTextEnd )
                                -- [])
                            -- (Right 604)
