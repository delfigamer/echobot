{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel.TgSpec
    ( spec
    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Req
import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.Text.Encoding as TextEncoding
import qualified System.IO as IO
import qualified Channel
import qualified Channel.Tg as Tg
import qualified Logger
import qualified WebDriver


withTestDriver
    :: (IORef (WebDriver.Address -> Value -> IO Value) -> WebDriver.Handle -> IO r)
    -> IO r
withTestDriver body = do
    phandler <- newIORef $ defaultReqHandler
    body phandler $ WebDriver.Handle
        { WebDriver.request = testDriverRequest phandler }


testDriverRequest
    :: (ToJSON a, FromJSON b)
    => IORef (WebDriver.Address -> Value -> IO Value)
    -> WebDriver.Address -> a -> IO b
testDriverRequest phandler address params = do
    handler <- readIORef phandler
    v <- handler address (toJSON params)
    case fromJSON v of
        Error err -> fail err
        Success x -> return $ x


defaultReqHandler :: WebDriver.Address -> Value -> IO Value
defaultReqHandler _ _ = fail "too many requests"


oneRequest
    :: (Show r, Eq r)
    => IORef (WebDriver.Address -> Value -> IO Value)
    -> WebDriver.Address
    -> Value
    -> Value
    -> IO r
    -> r
    -> IO ()
oneRequest phandler expaddress expparams v act expret = do
    ret <- newIORef $ Nothing
    writeIORef phandler $ \address params -> do
        address `shouldBe` expaddress
        params `shouldBe` expparams
        writeIORef phandler $ defaultReqHandler
        return $ v
    ret <- act
    ret `shouldBe` expret


oneRequestWithReplyMarkup
    :: (Show r, Eq r)
    => IORef (WebDriver.Address -> Value -> IO Value)
    -> WebDriver.Address
    -> Value
    -> Value
    -> Value
    -> IO r
    -> r
    -> IO ()
oneRequestWithReplyMarkup phandler expaddress expparams expmarkup v act expret = do
    ret <- newIORef $ Nothing
    writeIORef phandler $ \address params -> do
        address `shouldBe` expaddress
        Object parobj <- return $ params
        Just (String markupstr) <- return $ HashMap.lookup "reply_markup" parobj
        let parobj2 = HashMap.delete "reply_markup" parobj
        Object parobj2 `shouldBe` expparams
        Just markup <- return $ decodeStrict $ TextEncoding.encodeUtf8 markupstr
        markup `shouldBe` expmarkup
        writeIORef phandler $ defaultReqHandler
        return $ v
    ret <- act
    ret `shouldBe` expret


spec :: Spec
spec = do
    describe "Channel.Tg" $ do
        let token = "bottok"
        let timeout = 56
        let conf = Tg.Config
                { Tg.cToken = token
                , Tg.cTimeout = timeout
                , Tg.cKeyboardWidth = 3 }
        it "sends requests" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .= Array mempty
                                ])
                            (Channel.poll channel)
                            []
        it "keeps track of the offset" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object ["update_id" .= (1 :: Int)]
                                    , object ["update_id" .= (2 :: Int)]
                                    , object ["update_id" .= (3 :: Int)]
                                    ]
                                ])
                            (Channel.poll channel)
                            []
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["offset" .= (4 :: Int), "timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object ["update_id" .= (7 :: Int)]
                                    , object ["update_id" .= (4 :: Int)]
                                    ]
                                ])
                            (Channel.poll channel)
                            []
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["offset" .= (8 :: Int), "timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .= Array mempty
                                ])
                            (Channel.poll channel)
                            []
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["offset" .= (8 :: Int), "timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .= Array mempty
                                ])
                            (Channel.poll channel)
                            []
        it "receives text and sticker messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object
                                        [ "update_id" .= (1 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "message_id" .= (500 :: Int)
                                            , "text" .= ("Sample Text" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (2 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (101 :: Int)]
                                            , "message_id" .= (501 :: Int)
                                            , "text" .= ("Sample Text 2" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (3 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (102 :: Int)]
                                            , "sticker" .= object ["file_id" .= ("sid" :: Text)]
                                            ]
                                        ]
                                    ]
                                ])
                            (Channel.poll channel)
                            [ Channel.EventMessage 100 500 "Sample Text"
                            , Channel.EventMessage 101 501 "Sample Text 2"
                            , Channel.EventSticker 102 "sid"
                            ]
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["offset" .= (4 :: Int), "timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object
                                        [ "update_id" .= (4 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "message_id" .= (505 :: Int)
                                            , "text" .= ("Sample Text 3" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (5 :: Int)
                                        , "unknown_update" .= ("ignore me" :: Text)
                                        ]
                                    , object
                                        [ "update_id" .= (6 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (102 :: Int)]
                                            , "from" .= object ["id" .= (502 :: Int)]
                                            , "unknown_message" .= ("ignore_me" :: Text)
                                            ]
                                        ]
                                    ]
                                ])
                            (Channel.poll channel)
                            [ Channel.EventMessage 100 505 "Sample Text 3"
                            ]
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["offset" .= (7 :: Int), "timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .= Array mempty
                                ])
                            (Channel.poll channel)
                            []
        it "sends text messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "text" .= ("message" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= object ["message_id" .= (600 :: Int)]
                                ])
                            (Channel.sendMessage channel 100 "message" [])
                            (Right 600)
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            (object
                                [ "chat_id" .= (102 :: Int)
                                , "text" .= ("message 2" :: Text)
                                ])
                            (object
                                [ "ok" .= False
                                , "description" .= ("err" :: Text)
                                ])
                            (Channel.sendMessage channel 102 "message 2" [])
                            (Left "err")
        it "sends sticker messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendSticker"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "sticker" .= ("sticker" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendSticker channel 100 "sticker")
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendSticker"])
                            (object
                                [ "chat_id" .= (102 :: Int)
                                , "sticker" .= ("sticker 2" :: Text)
                                ])
                            (object
                                [ "ok" .= False
                                , "description" .= ("err" :: Text)
                                ])
                            (Channel.sendSticker channel 102 "sticker 2")
                            (Left "err")
        it "updates existing messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "editMessageText"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "message_id" .= (600 :: Int)
                                , "text" .= ("updated message" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.updateMessage channel 100 600 "updated message" [])
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "editMessageText"])
                            (object
                                [ "chat_id" .= (102 :: Int)
                                , "message_id" .= (700 :: Int)
                                , "text" .= ("updated message 2" :: Text)
                                ])
                            (object
                                [ "ok" .= False
                                , "description" .= ("err" :: Text)
                                ])
                            (Channel.updateMessage channel 102 700 "updated message 2" [])
                            (Left "err")
        it "sends messages with buttons" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequestWithReplyMarkup phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMessage"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "text" .= ("message" :: Text)
                                ])
                            (object
                                [ "inline_keyboard" .=
                                    [ [ object ["text" .= ("t1" :: Text), "callback_data" .= ("u1" :: Text)]
                                      , object ["text" .= ("t2" :: Text), "callback_data" .= ("u2" :: Text)]
                                      , object ["text" .= ("t3" :: Text), "callback_data" .= ("u3" :: Text)]
                                      ]
                                    , [ object ["text" .= ("t4" :: Text), "callback_data" .= ("u4" :: Text)]
                                      ]
                                    ]
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= object ["message_id" .= (600 :: Int)]
                                ])
                            (Channel.sendMessage channel 100 "message"
                                [ Channel.QueryButton "t1" "u1"
                                , Channel.QueryButton "t2" "u2"
                                , Channel.QueryButton "t3" "u3"
                                , Channel.QueryButton "t4" "u4"
                                ])
                            (Right 600)
        it "updates messages with buttons" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequestWithReplyMarkup phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "editMessageText"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "message_id" .= (600 :: Int)
                                , "text" .= ("updated message" :: Text)
                                ])
                            (object
                                [ "inline_keyboard" .=
                                    [ [ object ["text" .= ("t1" :: Text), "callback_data" .= ("u1" :: Text)]
                                      , object ["text" .= ("t2" :: Text), "callback_data" .= ("u2" :: Text)]
                                      , object ["text" .= ("t3" :: Text), "callback_data" .= ("u3" :: Text)]
                                      ]
                                    , [ object ["text" .= ("t4" :: Text), "callback_data" .= ("u4" :: Text)]
                                      , object ["text" .= ("t5" :: Text), "callback_data" .= ("u5" :: Text)]
                                      ]
                                    ]
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.updateMessage channel 100 600 "updated message"
                                [ Channel.QueryButton "t1" "u1"
                                , Channel.QueryButton "t2" "u2"
                                , Channel.QueryButton "t3" "u3"
                                , Channel.QueryButton "t4" "u4"
                                , Channel.QueryButton "t5" "u5"
                                ])
                            (Right ())
        it "receives button events" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object
                                        [ "update_id" .= (1 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "message_id" .= (500 :: Int)
                                            , "text" .= ("Sample Text" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (2 :: Int)
                                        , "callback_query" .= object
                                            [ "message" .= object
                                                [ "chat" .= object ["id" .= (101 :: Int)]
                                                , "message_id" .= (501 :: Int)
                                                ]
                                            , "id" .= ("qid1" :: Text)
                                            , "data" .= ("qdata1" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (3 :: Int)
                                        , "callback_query" .= object
                                            [ "message" .= object
                                                [ "chat" .= object ["id" .= (102 :: Int)]
                                                , "message_id" .= (502 :: Int)
                                                ]
                                            , "id" .= ("qid2" :: Text)
                                            , "data" .= ("qdata2" :: Text)
                                            ]
                                        ]
                                    ]
                                ])
                            (Channel.poll channel)
                            [ Channel.EventMessage 100 500 "Sample Text"
                            , Channel.EventQuery 101 501 "qid1" "qdata1"
                            , Channel.EventQuery 102 502 "qid2" "qdata2"
                            ]
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["offset" .= (4 :: Int), "timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object
                                        [ "update_id" .= (4 :: Int)
                                        , "callback_query" .= object
                                            [ "message" .= object
                                                [ "chat" .= object ["id" .= (110 :: Int)]
                                                , "message_id" .= (510 :: Int)
                                                ]
                                            , "id" .= ("qid5" :: Text)
                                            , "data" .= ("qdata5" :: Text)
                                            ]
                                        ]
                                    ]
                                ])
                            (Channel.poll channel)
                            [ Channel.EventQuery 110 510 "qid5" "qdata5"
                            ]
        it "sends answers to button queries" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "answerCallbackQuery"])
                            (object
                                [ "callback_query_id" .= ("qid1" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.answerQuery channel "qid1" "")
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "answerCallbackQuery"])
                            (object
                                [ "callback_query_id" .= ("qid2" :: Text)
                                , "text" .= ("sample text" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.answerQuery channel "qid2" "sample text")
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "answerCallbackQuery"])
                            (object
                                [ "callback_query_id" .= ("qid3" :: Text)
                                , "text" .= ("sample text 3" :: Text)
                                ])
                            (object
                                [ "ok" .= False
                                , "description" .= ("err" :: Text)
                                ])
                            (Channel.answerQuery channel "qid3" "sample text 3")
                            (Left "err")
        it "receives media messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object
                                        [ "update_id" .= (1 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "photo" .=
                                                [ object ["file_id" .= ("photo 1" :: Text)]
                                                ]
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (2 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "photo" .=
                                                [ object ["file_id" .= ("photo 2" :: Text)]
                                                ]
                                            , "caption" .= ("caption 2" :: Text)
                                            ]
                                        ]
                                    ]
                                ])
                            (Channel.poll channel)
                            [ Channel.EventMedia 100 "" (Channel.MediaPhoto "photo 1")
                            , Channel.EventMedia 100 "caption 2" (Channel.MediaPhoto "photo 2")
                            ]
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["offset" .= (3 :: Int), "timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object
                                        [ "update_id" .= (3 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "video" .= object ["file_id" .= ("video id" :: Text)]
                                            , "caption" .= ("video caption" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (4 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "audio" .= object ["file_id" .= ("audio id" :: Text)]
                                            , "caption" .= ("audio caption" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (5 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "animation" .= object ["file_id" .= ("animation id" :: Text)]
                                            , "document" .= object ["file_id" .= ("other id" :: Text)]
                                            , "caption" .= ("animation caption" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (6 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "voice" .= object ["file_id" .= ("voice id" :: Text)]
                                            , "caption" .= ("voice caption" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (7 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "document" .= object ["file_id" .= ("document id" :: Text)]
                                            , "caption" .= ("document caption" :: Text)
                                            ]
                                        ]
                                    ]
                                ])
                            (Channel.poll channel)
                            [ Channel.EventMedia 100 "video caption" $ Channel.MediaVideo "video id"
                            , Channel.EventMedia 100 "audio caption" $ Channel.MediaAudio "audio id"
                            , Channel.EventMedia 100 "animation caption" $ Channel.MediaAnimation "animation id"
                            , Channel.EventMedia 100 "voice caption" $ Channel.MediaVoice "voice id"
                            , Channel.EventMedia 100 "document caption" $ Channel.MediaDocument "document id"
                            ]
        it "receives media group messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "getUpdates"])
                            (object ["timeout" .= timeout])
                            (object
                                [ "ok" .= True
                                , "result" .=
                                    [ object
                                        [ "update_id" .= (1 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "photo" .=
                                                [ object ["file_id" .= ("photo 1" :: Text)]
                                                ]
                                            , "caption" .= ("caption 1" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (2 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "photo" .=
                                                [ object ["file_id" .= ("photo 2" :: Text)]
                                                ]
                                            , "caption" .= ("caption 2" :: Text)
                                            , "media_group_id" .= ("group id" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (3 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "photo" .=
                                                [ object ["file_id" .= ("photo 3" :: Text)]
                                                ]
                                            , "media_group_id" .= ("group id" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (4 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "video" .= object ["file_id" .= ("video 4" :: Text)]
                                            , "caption" .= ("caption 4" :: Text)
                                            , "media_group_id" .= ("group id" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (5 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "video" .= object ["file_id" .= ("video 5" :: Text)]
                                            , "caption" .= ("caption 5" :: Text)
                                            , "media_group_id" .= ("group id 2" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (6 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "video" .= object ["file_id" .= ("video 6" :: Text)]
                                            , "caption" .= ("caption 6" :: Text)
                                            , "media_group_id" .= ("group id 2" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (7 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (200 :: Int)]
                                            , "video" .= object ["file_id" .= ("video 7" :: Text)]
                                            , "caption" .= ("caption 7" :: Text)
                                            , "media_group_id" .= ("group id 2" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (8 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (200 :: Int)]
                                            , "video" .= object ["file_id" .= ("video 8" :: Text)]
                                            , "caption" .= ("caption 8" :: Text)
                                            , "media_group_id" .= ("group id 2" :: Text)
                                            ]
                                        ]
                                    , object
                                        [ "update_id" .= (5 :: Int)
                                        , "message" .= object
                                            [ "chat" .= object ["id" .= (100 :: Int)]
                                            , "photo" .=
                                                [ object ["file_id" .= ("photo 9" :: Text)]
                                                ]
                                            , "caption" .= ("caption 9" :: Text)
                                            ]
                                        ]
                                    ]
                                ])
                            (Channel.poll channel)
                            [ Channel.EventMedia 100 "caption 1" $ Channel.MediaPhoto "photo 1"
                            , Channel.EventMediaGroup 100 "group id"
                                $ Channel.MediaGroupPhoto "caption 2" "photo 2"
                                $ Channel.MediaGroupPhoto "" "photo 3"
                                $ Channel.MediaGroupVideo "caption 4" "video 4"
                                $ Channel.MediaGroupEnd
                            , Channel.EventMediaGroup 100 "group id 2"
                                $ Channel.MediaGroupVideo "caption 5" "video 5"
                                $ Channel.MediaGroupVideo "caption 6" "video 6"
                                $ Channel.MediaGroupEnd
                            , Channel.EventMediaGroup 200 "group id 2"
                                $ Channel.MediaGroupVideo "caption 7" "video 7"
                                $ Channel.MediaGroupVideo "caption 8" "video 8"
                                $ Channel.MediaGroupEnd
                            , Channel.EventMedia 100 "caption 9" $ Channel.MediaPhoto "photo 9"
                            ]
        it "sends media" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendPhoto"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "photo" .= ("photo 1" :: Text)
                                , "caption" .= ("caption 1" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMedia channel 100 "caption 1" (Channel.MediaPhoto "photo 1"))
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendVideo"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "video" .= ("video 2" :: Text)
                                , "caption" .= ("caption 2" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMedia channel 100 "caption 2" (Channel.MediaVideo "video 2"))
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendAudio"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "audio" .= ("audio 3" :: Text)
                                , "caption" .= ("caption 3" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMedia channel 100 "caption 3" (Channel.MediaAudio "audio 3"))
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendAnimation"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "animation" .= ("animation 4" :: Text)
                                , "caption" .= ("caption 4" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMedia channel 100 "caption 4" (Channel.MediaAnimation "animation 4"))
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendVoice"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "voice" .= ("voice 5" :: Text)
                                , "caption" .= ("caption 5" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMedia channel 100 "caption 5" (Channel.MediaVoice "voice 5"))
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendDocument"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "document" .= ("document 6" :: Text)
                                , "caption" .= ("caption 6" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMedia channel 100 "caption 6" (Channel.MediaDocument "document 6"))
                            (Right ())
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendPhoto"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "photo" .= ("photo 7" :: Text)
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMedia channel 100 "" (Channel.MediaPhoto "photo 7"))
                            (Right ())
        it "sends media groups" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \phandler driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        oneRequest phandler
                            (WebDriver.HttpsAddress "api.telegram.org" [token, "sendMediaGroup"])
                            (object
                                [ "chat_id" .= (100 :: Int)
                                , "media" .=
                                    [ object
                                        [ "type" .= ("photo" :: Text)
                                        , "media" .= ("photo 1" :: Text)
                                        ]
                                    , object
                                        [ "type" .= ("photo" :: Text)
                                        , "media" .= ("photo 2" :: Text)
                                        , "caption" .= ("caption 2" :: Text)
                                        ]
                                    , object
                                        [ "type" .= ("video" :: Text)
                                        , "media" .= ("video 3" :: Text)
                                        , "caption" .= ("caption 3" :: Text)
                                        ]
                                    ]
                                ])
                            (object
                                [ "ok" .= True
                                , "result" .= True
                                ])
                            (Channel.sendMediaGroup channel 100
                                $ Channel.MediaGroupPhoto "" "photo 1"
                                $ Channel.MediaGroupPhoto "caption 2" "photo 2"
                                $ Channel.MediaGroupVideo "caption 3" "video 3"
                                $ Channel.MediaGroupEnd)
                            (Right ())
