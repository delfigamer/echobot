module Channel.TgSpec
    ( spec
    ) where


import Data.Aeson
import Data.Text (Text)
import Test.Hspec
import qualified Channel
import qualified Channel.Tg as Tg
import qualified Logger
import qualified WebDriver
import WebDriver.Test


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
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= Array mempty
                                    ]
                            ]
                            (flip shouldBe $
                                [])
        it "keeps track of the offset" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .=
                                        [ object ["update_id" .= (1 :: Int)]
                                        , object ["update_id" .= (2 :: Int)]
                                        , object ["update_id" .= (3 :: Int)]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 4
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .=
                                        [ object ["update_id" .= (7 :: Int)]
                                        , object ["update_id" .= (4 :: Int)]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 8
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= Array mempty
                                    ]
                            ]
                            (flip shouldBe $
                                [])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 8
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= Array mempty
                                    ]
                            ]
                            (flip shouldBe $
                                [])
        it "receives plain text messages" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .=
                                        [ object
                                            [ "update_id" .= (2 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "message_id" .= (500 :: Int)
                                                , "text" .= ("Sample Text" :: Text)
                                                ]
                                            ]
                                        , object
                                            [ "update_id" .= (3 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (101 :: Int)]
                                                , "message_id" .= (501 :: Int)
                                                , "text" .= ("Sample Text 2" :: Text)
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 500 $ Channel.plainText "Sample Text"
                                , Channel.EventMessage 101 501 $ Channel.plainText "Sample Text 2"
                                ])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 4
                                ]
                                |>> object
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
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 505 $ Channel.plainText "Sample Text 3"
                                ])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 7
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= Array mempty
                                    ]
                            ]
                            (flip shouldBe $
                                [])
        it "sends text messages" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.sendMessage channel 100 (Channel.plainText "message") [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "text" $ "message"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= object ["message_id" .= (600 :: Int)]
                                    ]
                            ]
                            (flip shouldBe $
                                Right 600)
                        perform pexpectations
                            (Channel.sendMessage channel 102 (Channel.plainText "message 2") [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 102
                                , WebDriver.ParamText "text" $ "message 2"
                                ]
                                |>> object
                                    [ "ok" .= False
                                    , "description" .= ("err" :: Text)
                                    ]
                            ]
                            (flip shouldBe $
                                Left "err")
        it "updates existing messages" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.updateMessage channel 100 600 (Channel.plainText "updated message") [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/editMessageText")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamNum "message_id" $ 600
                                , WebDriver.ParamText "text" $ "updated message"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
        it "sends messages with buttons" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.sendMessage channel 100 (Channel.plainText "message")
                                [ Channel.QueryButton "t1" "u1"
                                , Channel.QueryButton "t2" "u2"
                                , Channel.QueryButton "t3" "u3"
                                , Channel.QueryButton "t4" "u4"
                                ])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "text" $ "message"
                                , WebDriver.ParamJson "reply_markup" $ object
                                    [ "inline_keyboard" .=
                                        [   [ object ["text" .= ("t1" :: Text), "callback_data" .= ("u1" :: Text)]
                                            , object ["text" .= ("t2" :: Text), "callback_data" .= ("u2" :: Text)]
                                            , object ["text" .= ("t3" :: Text), "callback_data" .= ("u3" :: Text)]
                                            ]
                                        ,   [ object ["text" .= ("t4" :: Text), "callback_data" .= ("u4" :: Text)]
                                            ]
                                        ]
                                    ]
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= object ["message_id" .= (600 :: Int)]
                                    ]
                            ]
                            (flip shouldBe $
                                Right 600)
        it "updates messages with buttons" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.updateMessage channel 100 600 (Channel.plainText "updated message")
                                [ Channel.QueryButton "t1" "u1"
                                , Channel.QueryButton "t2" "u2"
                                , Channel.QueryButton "t3" "u3"
                                , Channel.QueryButton "t4" "u4"
                                , Channel.QueryButton "t5" "u5"
                                ])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/editMessageText")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamNum "message_id" $ 600
                                , WebDriver.ParamText "text" $ "updated message"
                                , WebDriver.ParamJson "reply_markup" $ object
                                    [ "inline_keyboard" .=
                                        [   [ object ["text" .= ("t1" :: Text), "callback_data" .= ("u1" :: Text)]
                                            , object ["text" .= ("t2" :: Text), "callback_data" .= ("u2" :: Text)]
                                            , object ["text" .= ("t3" :: Text), "callback_data" .= ("u3" :: Text)]
                                            ]
                                        ,   [ object ["text" .= ("t4" :: Text), "callback_data" .= ("u4" :: Text)]
                                            , object ["text" .= ("t5" :: Text), "callback_data" .= ("u5" :: Text)]
                                            ]
                                        ]
                                    ]
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
        it "receives button events" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                ]
                                |>> object
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
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 500 (Channel.plainText "Sample Text")
                                , Channel.EventQuery 101 501 "qid1" "qdata1"
                                , Channel.EventQuery 102 502 "qid2" "qdata2"
                                ])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 4
                                ]
                                |>> object
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
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventQuery 110 510 "qid5" "qdata5"
                                ])
        it "sends answers to button queries" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.answerQuery channel "qid1" "")
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/answerCallbackQuery")
                                [ WebDriver.ParamText "callback_query_id" $ "qid1"
                                , WebDriver.ParamText "text" $ ""
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.answerQuery channel "qid2" "sample text")
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/answerCallbackQuery")
                                [ WebDriver.ParamText "callback_query_id" $ "qid2"
                                , WebDriver.ParamText "text" $ "sample text"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.answerQuery channel "qid2" "sample text")
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/answerCallbackQuery")
                                [ WebDriver.ParamText "callback_query_id" $ "qid2"
                                , WebDriver.ParamText "text" $ "sample text"
                                ]
                                |>> object
                                    [ "ok" .= False
                                    , "description" .= ("err" :: Text)
                                    ]
                            ]
                            (flip shouldBe $
                                Left "err")
        it "receives media messages" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                ]
                                |>> object
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
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 "" [Channel.ForeignMedia Channel.MediaPhoto "photo 1" ""]
                                , Channel.EventMedia 100 "caption 2" [Channel.ForeignMedia Channel.MediaPhoto "photo 2" ""]
                                ])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 3
                                ]
                                |>> object
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
                                                , "sticker" .= object ["file_id" .= ("sticker id" :: Text)]
                                                ]
                                            ]
                                        , object
                                            [ "update_id" .= (7 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "voice" .= object ["file_id" .= ("voice id" :: Text)]
                                                , "caption" .= ("voice caption" :: Text)
                                                ]
                                            ]
                                        , object
                                            [ "update_id" .= (8 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "document" .= object ["file_id" .= ("document id" :: Text)]
                                                , "caption" .= ("document caption" :: Text)
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 "video caption" [Channel.ForeignMedia Channel.MediaVideo "video id" ""]
                                , Channel.EventMedia 100 "audio caption" [Channel.ForeignMedia Channel.MediaAudio "audio id" ""]
                                , Channel.EventMedia 100 "animation caption" [Channel.ForeignMedia Channel.MediaAnimation "animation id" ""]
                                , Channel.EventMedia 100 "" [Channel.ForeignMedia Channel.MediaSticker "sticker id" ""]
                                , Channel.EventMedia 100 "voice caption" [Channel.ForeignMedia Channel.MediaVoice "voice id" ""]
                                , Channel.EventMedia 100 "document caption" [Channel.ForeignMedia Channel.MediaDocument "document id" ""]
                                ])
        it "receives media group messages" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                ]
                                |>> object
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
                                                , "media_group_id" .= ("group id 2" :: Text)
                                                ]
                                            ]
                                        , object
                                            [ "update_id" .= (6 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "photo" .=
                                                    [ object ["file_id" .= ("photo 6" :: Text)]
                                                    ]
                                                , "media_group_id" .= ("group id 2" :: Text)
                                                ]
                                            ]
                                        , object
                                            [ "update_id" .= (7 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (200 :: Int)]
                                                , "photo" .=
                                                    [ object ["file_id" .= ("photo 7" :: Text)]
                                                    ]
                                                , "media_group_id" .= ("group id 2" :: Text)
                                                ]
                                            ]
                                        , object
                                            [ "update_id" .= (8 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (200 :: Int)]
                                                , "video" .= object ["file_id" .= ("video 8" :: Text)]
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
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 "caption 1"
                                    [ Channel.ForeignMedia Channel.MediaPhoto "photo 1" ""
                                    ]
                                , Channel.EventMedia 100 "caption 2"
                                    [ Channel.ForeignMedia Channel.MediaPhoto "photo 2" ""
                                    , Channel.ForeignMedia Channel.MediaPhoto "photo 3" ""
                                    ]
                                , Channel.EventMedia 100 "caption 4"
                                    [ Channel.ForeignMedia Channel.MediaVideo "video 4" ""
                                    ]
                                , Channel.EventMedia 100 ""
                                    [ Channel.ForeignMedia Channel.MediaVideo "video 5" ""
                                    , Channel.ForeignMedia Channel.MediaPhoto "photo 6" ""
                                    ]
                                , Channel.EventMedia 200 ""
                                    [ Channel.ForeignMedia Channel.MediaPhoto "photo 7" ""
                                    , Channel.ForeignMedia Channel.MediaVideo "video 8" ""
                                    ]
                                , Channel.EventMedia 100 "caption 9"
                                    [ Channel.ForeignMedia Channel.MediaPhoto "photo 9" ""
                                    ]
                                ])
        it "sends media" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.sendMedia channel 100 "caption 1" [Channel.SendableMedia Channel.MediaPhoto "photo 1"])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendPhoto")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "photo" $ "photo 1"
                                , WebDriver.ParamText "caption" $ "caption 1"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.sendMedia channel 100 "" [Channel.SendableMedia Channel.MediaVideo "video 1"])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendVideo")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "video" $ "video 1"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.sendMedia channel 100 "" [Channel.SendableMedia Channel.MediaAudio "audio 1"])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendAudio")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "audio" $ "audio 1"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.sendMedia channel 100 "" [Channel.SendableMedia Channel.MediaAnimation "animation 1"])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendAnimation")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "animation" $ "animation 1"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.sendMedia channel 100 "" [Channel.SendableMedia Channel.MediaVoice "voice 1"])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendVoice")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "voice" $ "voice 1"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.sendMedia channel 100 "" [Channel.SendableMedia Channel.MediaSticker "sticker 1"])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendSticker")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "sticker" $ "sticker 1"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.sendMedia channel 100 "" [Channel.SendableMedia Channel.MediaDocument "document 1"])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendDocument")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "document" $ "document 1"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
        it "sends media groups" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.sendMedia channel 100 "caption 1"
                                [ Channel.SendableMedia Channel.MediaPhoto "photo 1-1"
                                , Channel.SendableMedia Channel.MediaPhoto "photo 1-2"
                                , Channel.SendableMedia Channel.MediaVideo "video 1-3"
                                ])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMediaGroup")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamJson "media" $ toJSON $
                                    [ object
                                        [ "type" .= ("photo" :: Text)
                                        , "media" .= ("photo 1-1" :: Text)
                                        , "caption" .= ("caption 1" :: Text)
                                        ]
                                    , object
                                        [ "type" .= ("photo" :: Text)
                                        , "media" .= ("photo 1-2" :: Text)
                                        ]
                                    , object
                                        [ "type" .= ("video" :: Text)
                                        , "media" .= ("video 1-3" :: Text)
                                        ]
                                    ]
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            ]
                            (flip shouldBe $
                                Right ())
                        perform pexpectations
                            (Channel.sendMedia channel 100 "caption 2"
                                [ Channel.SendableMedia Channel.MediaPhoto "photo 2-1"
                                , Channel.SendableMedia Channel.MediaVideo "video 2-2"
                                , Channel.SendableMedia Channel.MediaSticker "sticker 2-3"
                                , Channel.SendableMedia Channel.MediaVideo "video 2-4"
                                , Channel.SendableMedia Channel.MediaDocument "document 2-5"
                                , Channel.SendableMedia Channel.MediaVideo "video 2-6"
                                , Channel.SendableMedia Channel.MediaVideo "video 2-7"
                                ])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMediaGroup")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamJson "media" $ toJSON $
                                    [ object
                                        [ "type" .= ("photo" :: Text)
                                        , "media" .= ("photo 2-1" :: Text)
                                        , "caption" .= ("caption 2" :: Text)
                                        ]
                                    , object
                                        [ "type" .= ("video" :: Text)
                                        , "media" .= ("video 2-2" :: Text)
                                        ]
                                    ]
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            , Request
                                ("https://api.telegram.org/" <> token <> "/sendSticker")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "sticker" $ "sticker 2-3"
                                ]
                                |>> object
                                    [ "ok" .= False
                                    , "description" .= ("err 1" :: Text)
                                    ]
                            , Request
                                ("https://api.telegram.org/" <> token <> "/sendVideo")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "video" $ "video 2-4"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            , Request
                                ("https://api.telegram.org/" <> token <> "/sendDocument")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "document" $ "document 2-5"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= True
                                    ]
                            , Request
                                ("https://api.telegram.org/" <> token <> "/sendMediaGroup")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamJson "media" $ toJSON $
                                    [ object
                                        [ "type" .= ("video" :: Text)
                                        , "media" .= ("video 2-6" :: Text)
                                        ]
                                    , object
                                        [ "type" .= ("video" :: Text)
                                        , "media" .= ("video 2-7" :: Text)
                                        ]
                                    ]
                                ]
                                |>> object
                                    [ "ok" .= False
                                    , "description" .= ("err 2" :: Text)
                                    ]
                            ]
                            (flip shouldBe $
                                Left "err 1")
        it "receives rich text messages" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .=
                                        [ object
                                            [ "update_id" .= (1 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "message_id" .= (500 :: Int)
                                                {-                                                            ^23456789_12^23456789_      -}
                                                {-                  ^2345^234567^23456789_1      ^23456^23456789_123456789^23456789_12345 -}
                                                , "text" .= ("plain bold italic bolditalic plain under strike understrike boldunder under" :: Text)
                                                {-            _123456789_123456789_123456789_123456789_123456789_123456789_123456789_1234 -}
                                                , "entities" .=
                                                    [ object ["type" .= ("bold" :: Text), "offset" .= (6 :: Int), "length" .= (5 :: Int)]
                                                    , object ["type" .= ("italic" :: Text), "offset" .= (11 :: Int), "length" .= (7 :: Int)]
                                                    , object ["type" .= ("bold" :: Text), "offset" .= (18 :: Int), "length" .= (11 :: Int)]
                                                    , object ["type" .= ("italic" :: Text), "offset" .= (18 :: Int), "length" .= (11 :: Int)]
                                                    , object ["type" .= ("underline" :: Text), "offset" .= (35 :: Int), "length" .= (6 :: Int)]
                                                    , object ["type" .= ("strikethrough" :: Text), "offset" .= (41 :: Int), "length" .= (19 :: Int)]
                                                    , object ["type" .= ("underline" :: Text), "offset" .= (48 :: Int), "length" .= (12 :: Int)]
                                                    , object ["type" .= ("underline" :: Text), "offset" .= (60 :: Int), "length" .= (15 :: Int)]
                                                    , object ["type" .= ("bold" :: Text), "offset" .= (60 :: Int), "length" .= (10 :: Int)]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 500
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                    $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False True False False) "italic "
                                    $ Channel.RichTextSpan (Channel.SpanStyle True True False False) "bolditalic "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False True) "strike "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False True True) "understrike "
                                    $ Channel.RichTextSpan (Channel.SpanStyle True False True False) "boldunder "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                    $ Channel.RichTextEnd
                                ])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 2
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .=
                                        [ object
                                            [ "update_id" .= (2 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "message_id" .= (500 :: Int)
                                                {-                 ^234                    ^2345 -}
                                                {-            ^23456789_1234       ^23456789_123 -}
                                                , "text" .= ("link bold link plain mention under" :: Text)
                                                {-            _123456789_123456789_123456789_123 -}
                                                , "entities" .=
                                                    [ object ["type" .= ("bold" :: Text), "offset" .= (5 :: Int), "length" .= (4 :: Int)]
                                                    , object ["type" .= ("underline" :: Text), "offset" .= (29 :: Int), "length" .= (5 :: Int)]
                                                    , object ["type" .= ("text_link" :: Text), "offset" .= (0 :: Int), "length" .= (14 :: Int), "url" .= ("link url" :: Text)]
                                                    , object ["type" .= ("text_mention" :: Text), "offset" .= (21 :: Int), "length" .= (13 :: Int), "user" .= object ["id" .= ("user id" :: Text)]]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 500
                                    $ Channel.RichTextLink "link url"
                                        ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "link "
                                        $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold"
                                        $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " link"
                                        $ Channel.RichTextEnd )
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                    $ Channel.RichTextMention "user id"
                                        ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "mention "
                                        $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                        $ Channel.RichTextEnd )
                                    $ Channel.RichTextEnd
                                ])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 3
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .=
                                        [ object
                                            [ "update_id" .= (3 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "message_id" .= (500 :: Int)
                                                {-            ^23456789_1       ^23456789_       ^23456789_12345 -}
                                                , "text" .= ("inline-code plain block-code plain block-code-lang" :: Text)
                                                {-            _123456789_123456789_123456789_123456789_123456789 -}
                                                , "entities" .=
                                                    [ object ["type" .= ("code" :: Text), "offset" .= (0 :: Int), "length" .= (11 :: Int)]
                                                    , object ["type" .= ("ignore me" :: Text), "offset" .= (14 :: Int), "length" .= (4 :: Int)]
                                                    , object ["type" .= ("pre" :: Text), "offset" .= (18 :: Int), "length" .= (10 :: Int)]
                                                    , object ["type" .= ("pre" :: Text), "offset" .= (35 :: Int), "length" .= (15 :: Int), "language" .= ("code lang" :: Text)]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 500
                                    $ Channel.RichTextMono "inline-code"
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                    $ Channel.RichTextCode "" "block-code"
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                    $ Channel.RichTextCode "code lang" "block-code-lang"
                                    $ Channel.RichTextEnd
                                ])
                        perform pexpectations
                            (Channel.poll channel)
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/getUpdates")
                                [ WebDriver.ParamNum "timeout" $ timeout
                                , WebDriver.ParamNum "offset" $ 4
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .=
                                        [ object
                                            [ "update_id" .= (4 :: Int)
                                            , "message" .= object
                                                [ "chat" .= object ["id" .= (100 :: Int)]
                                                , "message_id" .= (500 :: Int)
                                                {-                       ^     345                ^2 -}
                                                , "text" .= ("tt \x1F914\x1F914 bb \x1F914\x1F914 uu tt" :: Text)
                                                {-            _12 3      5     789_ 1      3     567 -}
                                                , "entities" .=
                                                    [ object ["type" .= ("bold" :: Text), "offset" .= (5 :: Int), "length" .= (5 :: Int)]
                                                    , object ["type" .= ("underline" :: Text), "offset" .= (16 :: Int), "length" .= (2 :: Int)]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 500
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "tt \x1F914"
                                    $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "\x1F914 bb"
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " \x1F914\x1F914 "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "uu"
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " tt"
                                    $ Channel.RichTextEnd
                                ])
        it "sends rich text messages" $ do
            Logger.withTestLogger $ \logger -> do
                withTestDriver $ \pexpectations driver -> do
                    Tg.withTgChannel conf logger driver $ \channel -> do
                        perform pexpectations
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold "
                                $ Channel.RichTextSpan (Channel.SpanStyle False True False False) "italic "
                                $ Channel.RichTextSpan (Channel.SpanStyle True True False False) "bolditalic "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False True) "strike "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False True True) "understrike "
                                $ Channel.RichTextSpan (Channel.SpanStyle True False True False) "boldunder "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                $ Channel.RichTextEnd )
                                [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "parse_mode" $ "HTML"
                                , WebDriver.ParamText "text" $ "\
                                    \plain \
                                    \<b>bold </b>\
                                    \<i>italic </i>\
                                    \<b><i>bolditalic </i></b>\
                                    \plain \
                                    \<u>under </u>\
                                    \<s>strike </s>\
                                    \<u><s>understrike </s></u>\
                                    \<b><u>boldunder </u></b>\
                                    \<u>under</u>"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= object ["message_id" .= (600 :: Int)]
                                    ]
                            ]
                            (flip shouldBe $
                                Right 600)
                        perform pexpectations
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextLink "link url"
                                    ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "link "
                                    $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold"
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " link"
                                    $ Channel.RichTextEnd )
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextMention "user id"
                                    ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "mention "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                    $ Channel.RichTextEnd )
                                $ Channel.RichTextEnd )
                                [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "parse_mode" $ "HTML"
                                , WebDriver.ParamText "text" $ "\
                                    \<a href=\"link url\">link <b>bold</b> link</a>\
                                    \ plain \
                                    \<a href=\"tg://user?id=user id\">mention <u>under</u></a>"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= object ["message_id" .= (600 :: Int)]
                                    ]
                            ]
                            (flip shouldBe $
                                Right 600)
                        perform pexpectations
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextMono "inline-code"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextCode "" "block-code"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextCode "code lang" "block-code-lang"
                                $ Channel.RichTextEnd )
                                [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "parse_mode" $ "HTML"
                                , WebDriver.ParamText "text" $ "\
                                    \<code>inline-code</code>\
                                    \ plain \
                                    \<pre>block-code</pre>\
                                    \ plain \
                                    \<pre><code class=\"language-code lang\">block-code-lang</code></pre>"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= object ["message_id" .= (600 :: Int)]
                                    ]
                            ]
                            (flip shouldBe $
                                Right 600)
                        perform pexpectations
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "tt \x1F914"
                                $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "\x1F914 bb"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " \x1F914\x1F914 "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "uu"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " tt"
                                $ Channel.RichTextEnd )
                                [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "parse_mode" $ "HTML"
                                , WebDriver.ParamText "text" $ "\
                                    \tt \x1F914\
                                    \<b>\x1F914 bb</b>\
                                    \ \x1F914\x1F914 \
                                    \<u>uu</u>\
                                    \ tt"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= object ["message_id" .= (600 :: Int)]
                                    ]
                            ]
                            (flip shouldBe $
                                Right 600)
                        perform pexpectations
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "abc<>&\""
                                $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "abc<>&\""
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "abc<>&\""
                                $ Channel.RichTextEnd )
                                [])
                            [ Request
                                ("https://api.telegram.org/" <> token <> "/sendMessage")
                                [ WebDriver.ParamNum "chat_id" $ 100
                                , WebDriver.ParamText "parse_mode" $ "HTML"
                                , WebDriver.ParamText "text" $ "\
                                    \abc&lt;&gt;&amp;&quot;\
                                    \<b>abc&lt;&gt;&amp;&quot;</b>\
                                    \abc&lt;&gt;&amp;&quot;"
                                ]
                                |>> object
                                    [ "ok" .= True
                                    , "result" .= object ["message_id" .= (600 :: Int)]
                                    ]
                            ]
                            (flip shouldBe $
                                Right 600)
