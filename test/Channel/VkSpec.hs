{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Channel.VkSpec
    ( spec
    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Aeson.Text
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import Network.HTTP.Req
import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as EncodingLazy
import qualified System.IO as IO
import qualified System.Random as Random
import qualified Channel
import qualified Channel.Vk.Internal as Vk
import qualified Logger
import qualified WebDriver


data ExpectedRequest
    = ExpectedRequest !WebDriver.Address [WebDriver.Param] !Value
    | ExpectedDownload !Text.Text !BSL.ByteString
    deriving (Show)


withTestDriver
    :: (IORef [ExpectedRequest] -> WebDriver.Handle -> IO r)
    -> IO r
withTestDriver body = do
    pbuf <- newIORef []
    body pbuf $ WebDriver.Handle
            { WebDriver.request = testDriverRequest pbuf
            , WebDriver.download = testDriverDownload pbuf
            }


testDriverRequest
    :: (FromJSON b)
    => IORef [ExpectedRequest] -> WebDriver.Address -> [WebDriver.Param] -> IO b
testDriverRequest pbuf address params = do
    ExpectedRequest address2 params2 result:rest <- readIORef pbuf
    (address, normalizeParamSet params) `shouldBe` (address2, normalizeParamSet params2)
    writeIORef pbuf $! rest
    case fromJSON result of
        Error err -> fail err
        Success x -> return $ x


testDriverDownload
    :: IORef [ExpectedRequest] -> Text.Text -> IO BSL.ByteString
testDriverDownload pbuf address = do
    ExpectedDownload address2 result:rest <- readIORef pbuf
    address `shouldBe` address2
    writeIORef pbuf $! rest
    return $ result


normalizeParamSet :: [WebDriver.Param] -> [WebDriver.Param]
normalizeParamSet params = sortOn WebDriver.paramName $ map normalize $ params
    where
    normalize (WebDriver.ParamTextLazy name text) = WebDriver.ParamText name $ TextLazy.toStrict text
    normalize (WebDriver.ParamNum name num) = WebDriver.ParamText name $ Text.pack $ show num
    normalize param = param


perform
    :: (Show r, Eq r)
    => IORef [ExpectedRequest]
    -> IO r
    -> [ExpectedRequest]
    -> (r -> IO q)
    -> IO q
perform pbuf act reqs rettest = do
    writeIORef pbuf $! reqs
    ret <- act
    bufrest <- readIORef pbuf
    bufrest `shouldSatisfy` null
    rettest ret


spec :: Spec
spec = do
    describe "Channel.Vk" $ do
        let token = "bottok"
        let groupId = 110
        let timeout = 56
        let conf = Vk.Config
                { Vk.cToken = token
                , Vk.cGroupId = groupId
                , Vk.cTimeout = timeout
                , Vk.cKeyboardWidth = 3 }
        let groupChatId x = 2000000000 + x
        let randomSeed = 1
        let randoms = map toInteger $ (Random.randoms $ Random.mkStdGen randomSeed :: [Word])
        it "performs a long poll" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://api.vk.com/method/groups.getLongPollServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "group_id" $ groupId
                                ]
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "1"
                                        ]
                                    ])
                            , ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "1"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "1"
                                    , "updates" .= Array mempty
                                    ])
                            ]
                            (flip shouldBe $
                                [])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "1"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "1"
                                    , "updates" .= Array mempty
                                    ])
                            ]
                            (flip shouldBe $
                                [])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "1"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "20"
                                    , "failed" .= Number 1
                                    ])
                            ]
                            (flip shouldBe $
                                [])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "20"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "failed" .= Number 2
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/groups.getLongPollServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "group_id" $ groupId
                                ]
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey2"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "30"
                                        ]
                                    ])
                            , ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey2"
                                , WebDriver.ParamText "ts" $ "30"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "30"
                                    , "updates" .= Array mempty
                                    ])
                            ]
                            (flip shouldBe $
                                [])
        it "receives plain text messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://api.vk.com/method/groups.getLongPollServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "group_id" $ groupId
                                ]
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "10"
                                        ]
                                    ])
                            , ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "10"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
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
                            (flip shouldBe $
                                [ Channel.EventMessage 100 10 $ Channel.plainText "100 message text"
                                , Channel.EventMessage 200 11 $ Channel.plainText "200 message text"
                                ])
        it "receives media messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://api.vk.com/method/groups.getLongPollServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "group_id" $ groupId
                                ]
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "10"
                                        ]
                                    ])
                            , ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "10"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
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
                            (flip shouldBe $
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
                                    , Channel.ForeignMedia Channel.MediaPhoto "!photo18_40_efef" "https://srv.userapi.com/group/file7.jpg"
                                    ]
                                ])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "20"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
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
                            (flip shouldBe $
                                [ Channel.EventMedia 100 "100 message text"
                                    [ Channel.ForeignMedia Channel.MediaVideo "video50_60" ""
                                    , Channel.ForeignMedia Channel.MediaVideo "video70_80_beef" ""
                                    ]
                                , Channel.EventMedia (groupChatId 300) ""
                                    [ Channel.ForeignMedia Channel.MediaVideo "video17_30" ""
                                    , Channel.ForeignMedia Channel.MediaVideo "!video18_40_efef" ""
                                    ]
                                ])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "30"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "40"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "audio"
                                                            , "audio" .= object
                                                                [ "owner_id" .= Number 50
                                                                , "id" .= Number 60
                                                                , "url" .= String "https://psv1.vkuseraudio.net/c1/u2/audios/abcd.mp3?extra=_1_2_3&long_chunk=1"
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "audio"
                                                            , "audio" .= object
                                                                [ "owner_id" .= Number 70
                                                                , "id" .= Number 80
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
                                                            [ "type" .= String "audio"
                                                            , "audio" .= object
                                                                [ "owner_id" .= Number 17
                                                                , "id" .= Number 30
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 ""
                                    [ Channel.ForeignMedia Channel.MediaAudio "audio50_60" "https://psv1.vkuseraudio.net/c1/u2/audios/abcd.mp3?extra=_1_2_3&long_chunk=1"
                                    , Channel.ForeignMedia Channel.MediaAudio "audio70_80" ""
                                    ]
                                , Channel.EventMedia (groupChatId 300) ""
                                    [ Channel.ForeignMedia Channel.MediaAudio "audio17_30" ""
                                    ]
                                ])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "40"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "50"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "doc"
                                                            , "doc" .= object
                                                                [ "owner_id" .= Number 50
                                                                , "id" .= Number 60
                                                                , "url" .= String "https://vk.com/doc50_60?hash=1&dl=2&api=1&no_preview=1"
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "doc"
                                                            , "doc" .= object
                                                                [ "owner_id" .= Number 70
                                                                , "id" .= Number 80
                                                                , "access_key" .= String "abab"
                                                                , "url" .= String "https://vk.com/doc70_80?hash=1&dl=2&api=1&no_preview=1"
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
                                                            [ "type" .= String "doc"
                                                            , "doc" .= object
                                                                [ "owner_id" .= Number 17
                                                                , "id" .= Number 30
                                                                , "url" .= String "https://vk.com/doc17_30?hash=1&dl=2&api=1&no_preview=1"
                                                                ]
                                                            ]
                                                        , object
                                                            [ "type" .= String "doc"
                                                            , "doc" .= object
                                                                [ "owner_id" .= Number 18
                                                                , "id" .= Number 40
                                                                , "access_key" .= String "efef"
                                                                , "url" .= String "https://vk.com/doc18_40?hash=1&dl=2&api=1&no_preview=1"
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 ""
                                    [ Channel.ForeignMedia Channel.MediaDocument "doc50_60" "\"a\"https://vk.com/doc50_60?hash=1&dl=2&api=1&no_preview=1"
                                    , Channel.ForeignMedia Channel.MediaDocument "" "\"a\"https://vk.com/doc70_80?hash=1&dl=2&api=1&no_preview=1"
                                    ]
                                , Channel.EventMedia (groupChatId 300) ""
                                    [ Channel.ForeignMedia Channel.MediaDocument "doc17_30" "\"a\"https://vk.com/doc17_30?hash=1&dl=2&api=1&no_preview=1"
                                    , Channel.ForeignMedia Channel.MediaDocument "" "\"a\"https://vk.com/doc18_40?hash=1&dl=2&api=1&no_preview=1"
                                    ]
                                ])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "50"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "60"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "sticker"
                                                            , "sticker" .= object
                                                                [ "sticker_id" .= Number 450
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 ""
                                    [ Channel.ForeignMedia Channel.MediaSticker "450" ""
                                    ]
                                ])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "60"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "70"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "audio_message"
                                                            , "audio_message" .= object
                                                                [ "owner_id" .= Number 50
                                                                , "id" .= Number 60
                                                                , "access_key" .= String "abab"
                                                                , "link_mp3" .= String "https://psv1.userapi.com/c1//u2/audiomsg/d1/50.mp3"
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
                                                            [ "type" .= String "audio_message"
                                                            , "audio_message" .= object
                                                                [ "owner_id" .= Number 18
                                                                , "id" .= Number 40
                                                                , "access_key" .= String "efef"
                                                                , "link_mp3" .= String "https://psv1.userapi.com/c1//u2/audiomsg/d1/60.mp3"
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 ""
                                    [ Channel.ForeignMedia Channel.MediaVoice "" "https://psv1.userapi.com/c1//u2/audiomsg/d1/50.mp3"
                                    ]
                                , Channel.EventMedia (groupChatId 300) ""
                                    [ Channel.ForeignMedia Channel.MediaVoice "" "https://psv1.userapi.com/c1//u2/audiomsg/d1/60.mp3"
                                    ]
                                ])
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "70"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "80"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "attachments" .=
                                                        [ object
                                                            [ "type" .= String "unknown_type"
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                [ Channel.EventMedia 100 ""
                                    [ Channel.ForeignMedia Channel.MediaUnknown "unknown_type" ""
                                    ]
                                ])
                        -- {- attachents with an access_key that come from group chats are busted, see Channel.Vk.Internal for a more detailed explanation -}
        it "re-posesses photos" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaPhoto "photo17_30_cdcd" "https://srv.userapi.com/group/file6.jpg"))
                            []
                            (flip shouldBe $
                                Channel.PossessMediaSuccess $ Channel.SendableMedia Channel.MediaPhoto "photo17_30_cdcd")
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaPhoto "!photo18_40_efef" "https://srv.userapi.com/group/file7.jpg"))
                            [ ExpectedRequest
                                "https://api.vk.com/method/photos.getMessagesUploadServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                ]
                                (object
                                    [ "response" .= object
                                        [ "upload_url" .= String "https://pu.vk.com/c6/ss7/upload.php?act=do_add&mid=1&aid=2&gid=3&hash=4&rhash=5&swfupload=1&api=1&mailphoto=1"
                                        ]
                                    ])
                            , ExpectedDownload
                                "https://srv.userapi.com/group/file7.jpg"
                                "<contents of file7.jpg>"
                            , ExpectedRequest
                                "https://pu.vk.com/c6/ss7/upload.php?act=do_add&mid=1&aid=2&gid=3&hash=4&rhash=5&swfupload=1&api=1&mailphoto=1"
                                [ WebDriver.ParamFile "file" "file7.jpg" $ "<contents of file7.jpg>"
                                ]
                                (object
                                    [ "server" .= Number 1234
                                    , "photo" .= String "{\"key\":\"value\"}"
                                    , "hash" .= String "56ff"
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/photos.saveMessagesPhoto"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "server" $ 1234
                                , WebDriver.ParamText "photo" $ "{\"key\":\"value\"}"
                                , WebDriver.ParamText "hash" $ "56ff"
                                ]
                                (object
                                    [ "response" .=
                                        [ object
                                            [ "owner_id" .= Number 10
                                            , "id" .= Number 67
                                            , "access_key" .= String "acdf"
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                Channel.PossessMediaSuccess $ Channel.SendableMedia Channel.MediaPhoto "photo10_67_acdf")
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaPhoto "!photo18_40_efef" "https://srv.userapi.com/group/file7.jpg"))
                            [ ExpectedRequest
                                "https://api.vk.com/method/photos.getMessagesUploadServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                ]
                                (object
                                    [ "response" .= object
                                        [ "upload_url" .= String "https://pu.vk.com/c6/ss7/upload.php?act=do_add&mid=1&aid=2&gid=3&hash=4&rhash=5&swfupload=1&api=1&mailphoto=1"
                                        ]
                                    ])
                            , ExpectedDownload
                                "https://srv.userapi.com/group/file7.jpg"
                                "<contents of file7.jpg>"
                            , ExpectedRequest
                                "https://pu.vk.com/c6/ss7/upload.php?act=do_add&mid=1&aid=2&gid=3&hash=4&rhash=5&swfupload=1&api=1&mailphoto=1"
                                [ WebDriver.ParamFile "file" "file7.jpg" $ "<contents of file7.jpg>"
                                ]
                                (object
                                    [ "server" .= Number 1234
                                    , "photo" .= String "{\"key\":\"value\"}"
                                    , "hash" .= String "56ff"
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/photos.saveMessagesPhoto"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "server" $ 1234
                                , WebDriver.ParamText "photo" $ "{\"key\":\"value\"}"
                                , WebDriver.ParamText "hash" $ "56ff"
                                ]
                                (object
                                    [ "response" .= Array mempty
                                    ])
                            ]
                            (flip shouldBe $
                                Channel.PossessMediaInternalError)
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaPhoto "!photo18_40_efef" "https://srv.userapi.com/group/file7.jpg"))
                            [ ExpectedRequest
                                "https://api.vk.com/method/photos.getMessagesUploadServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                ]
                                (object
                                    [ "response" .= object
                                        [ "upload_url" .= String "https://pu.vk.com/c6/ss7/upload.php?act=do_add&mid=1&aid=2&gid=3&hash=4&rhash=5&swfupload=1&api=1&mailphoto=1"
                                        ]
                                    ])
                            , ExpectedDownload
                                "https://srv.userapi.com/group/file7.jpg"
                                "<contents of file7.jpg>"
                            , ExpectedRequest
                                "https://pu.vk.com/c6/ss7/upload.php?act=do_add&mid=1&aid=2&gid=3&hash=4&rhash=5&swfupload=1&api=1&mailphoto=1"
                                [WebDriver.ParamFile "file" "file7.jpg" "<contents of file7.jpg>"]
                                (object
                                    [ "server" .= Number 1234
                                    , "photo" .= String "{\"key\":\"value\"}"
                                    , "hash" .= String "56ff"
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/photos.saveMessagesPhoto"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "server" $ 1234
                                , WebDriver.ParamText "photo" $ "{\"key\":\"value\"}"
                                , WebDriver.ParamText "hash" $ "56ff"
                                ]
                                (object
                                    [ "error" .= object ["error_msg" .= String "failed"]
                                    ])
                            ]
                            (flip shouldBe $
                                Channel.PossessMediaInternalError)
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaPhoto "!photo18_40_efef" "https://srv.userapi.com/group/file7.jpg"))
                            [ ExpectedRequest
                                "https://api.vk.com/method/photos.getMessagesUploadServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                ]
                                (object
                                    [ "error" .= object ["error_msg" .= String "failed"]
                                    ])
                            ]
                            (flip shouldBe $
                                Channel.PossessMediaInternalError)
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaPhoto "!photo18_40_efef" ""))
                            []
                            (flip shouldBe $
                                Channel.PossessMediaUnsupported)
        it "re-posesses documents" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaDocument "" "\"mydoc.txt\"https://vk.com/doc18_40?hash=1&dl=2&api=1&no_preview=1"))
                            [ ExpectedRequest
                                "https://api.vk.com/method/docs.getMessagesUploadServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamText "type" $ "doc"
                                ]
                                (object
                                    [ "response" .= object
                                        [ "upload_url" .= String "https://pu.vk.com/c1/upload.php?act=add_doc_new&mid=2&aid=-1&gid=0&type=0&peer_id=0&rhash=3&api=1&server=4"
                                        ]
                                    ])
                            , ExpectedDownload
                                "https://vk.com/doc18_40?hash=1&dl=2&api=1&no_preview=1"
                                "<contents of doc18_40>"
                            , ExpectedRequest
                                "https://pu.vk.com/c1/upload.php?act=add_doc_new&mid=2&aid=-1&gid=0&type=0&peer_id=0&rhash=3&api=1&server=4"
                                [ WebDriver.ParamFile "file" "mydoc.txt" $ "<contents of doc18_40>"
                                ]
                                (object
                                    [ "file" .= String "newdoc|5678cdef"
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/docs.save"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamText "file" $ "newdoc|5678cdef"
                                ]
                                (object
                                    [ "response" .= object
                                        [ "type" .= String "doc"
                                        , "doc" .= object
                                            [ "owner_id" .= Number 10
                                            , "id" .= Number 67
                                            , "access_key" .= String "acdf"
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                Channel.PossessMediaSuccess $ Channel.SendableMedia Channel.MediaDocument "doc10_67_acdf")
        it "re-posesses voice messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaVoice "" "https://psv1.userapi.com/c1//u2/audiomsg/d1/50.mp3"))
                            [ ExpectedRequest
                                "https://api.vk.com/method/docs.getMessagesUploadServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamText "type" $ "audio_message"
                                ]
                                (object
                                    [ "response" .= object
                                        [ "upload_url" .= String "https://pu.vk.com/c1/upload.php?act=add_doc_new&mid=2&aid=-1&gid=0&type=0&peer_id=0&rhash=3&api=1&server=4"
                                        ]
                                    ])
                            , ExpectedDownload
                                "https://psv1.userapi.com/c1//u2/audiomsg/d1/50.mp3"
                                "<contents of 50.mp3>"
                            , ExpectedRequest
                                "https://pu.vk.com/c1/upload.php?act=add_doc_new&mid=2&aid=-1&gid=0&type=0&peer_id=0&rhash=3&api=1&server=4"
                                [ WebDriver.ParamFile "file" "50.mp3" $ "<contents of 50.mp3>"
                                ]
                                (object
                                    [ "file" .= String "newvoice|9abc"
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/docs.save"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamText "file" $ "newvoice|9abc"
                                ]
                                (object
                                    [ "response" .= object
                                        [ "type" .= String "audio_message"
                                        , "audio_message" .= object
                                            [ "owner_id" .= Number 11
                                            , "id" .= Number 68
                                            , "access_key" .= String "bcde"
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                Channel.PossessMediaSuccess $ Channel.SendableMedia Channel.MediaVoice "doc11_68_bcde")
        it "doesn't re-posess unknown media types" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.possessMedia channel 100 (Channel.ForeignMedia Channel.MediaUnknown "unknown" "https://example.test/a.txt"))
                            []
                            (flip shouldBe $
                                Channel.PossessMediaUnknownType "unknown")
        it "sends text messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.sendMessage channel 100 (Channel.plainText "message text") [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamText "message" $ "message text"
                                , WebDriver.ParamNum "random_id" $ randoms !! 0
                                ]
                                (object
                                    [ "response" .= Number 15
                                    ])
                            ]
                            (flip shouldBe $
                                Right 15)
                        perform prequestbuf
                            (Channel.sendMessage channel 100 (Channel.plainText "message text") [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamText "message" $ "message text"
                                , WebDriver.ParamNum "random_id" $ randoms !! 1
                                ]
                                (object
                                    [ "error" .= object
                                        [ "error_msg" .= String "too bad"
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                Left "too bad")
        it "updates existing messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.updateMessage channel 100 15 (Channel.plainText "message text") [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.edit"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "message_id" $ 15
                                , WebDriver.ParamText "message" $ "message text"
                                ]
                                (object
                                    [ "response" .= True
                                    ])
                            ]
                            (flip shouldBe $
                                Right ())
                        perform prequestbuf
                            (Channel.updateMessage channel 100 0 (Channel.plainText "message text 2") [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 0
                                , WebDriver.ParamText "message" $ "message text 2"
                                ]
                                (object
                                    [ "response" .= Number 20
                                    ])
                            ]
                            (flip shouldBe $
                                Right ())
        it "sends messages with buttons" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.sendMessage channel 100 (Channel.plainText "message text")
                                [ Channel.QueryButton "t1" "u1"
                                , Channel.QueryButton "t2" "u2"
                                , Channel.QueryButton "t3" "u3"
                                , Channel.QueryButton "t4" "u4"
                                ])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamText "message" $ "message text"
                                , WebDriver.ParamNum "random_id" $ randoms !! 0
                                , WebDriver.ParamJson "keyboard" $ object
                                    [ "one_time" .= True
                                    , "buttons" .=
                                        [   [ object
                                                [ "action" .= object
                                                    ["type" .= String "text"
                                                    , "label" .= String "t1"
                                                    , "payload" .= String "\"u1\""
                                                    ]
                                                ]
                                            , object
                                                [ "action" .= object
                                                    [ "type" .= String "text"
                                                    , "label" .= String "t2"
                                                    , "payload" .= String "\"u2\""
                                                    ]
                                                ]
                                            , object
                                                [ "action" .= object
                                                    [ "type" .= String "text"
                                                    , "label" .= String "t3"
                                                    , "payload" .= String "\"u3\""
                                                    ]
                                                ]
                                            ]
                                        ,   [ object
                                                [ "action" .= object
                                                    [ "type" .= String "text"
                                                    , "label" .= String "t4"
                                                    , "payload" .= String "\"u4\""
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                                (object
                                    [ "response" .= Number 15
                                    ])
                            ]
                            (flip shouldBe $
                                Right 15)
        it "updates messages with buttons" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.updateMessage channel 100 15 (Channel.plainText "message text")
                                [ Channel.QueryButton "t1" "u1"
                                , Channel.QueryButton "t2" "u2"
                                , Channel.QueryButton "t3" "u3"
                                , Channel.QueryButton "t4" "u4"
                                ])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.edit"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "message_id" $ 15
                                , WebDriver.ParamText "message" $ "message text"
                                , WebDriver.ParamJson "keyboard" $ object
                                    [ "one_time" .= True
                                    , "buttons" .=
                                        [   [ object
                                                [ "action" .= object
                                                    ["type" .= String "text"
                                                    , "label" .= String "t1"
                                                    , "payload" .= String "\"u1\""
                                                    ]
                                                ]
                                            , object
                                                [ "action" .= object
                                                    [ "type" .= String "text"
                                                    , "label" .= String "t2"
                                                    , "payload" .= String "\"u2\""
                                                    ]
                                                ]
                                            , object
                                                [ "action" .= object
                                                    [ "type" .= String "text"
                                                    , "label" .= String "t3"
                                                    , "payload" .= String "\"u3\""
                                                    ]
                                                ]
                                            ]
                                        ,   [ object
                                                [ "action" .= object
                                                    [ "type" .= String "text"
                                                    , "label" .= String "t4"
                                                    , "payload" .= String "\"u4\""
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                                (object
                                    [ "response" .= True
                                    ])
                            ]
                            (flip shouldBe $
                                Right ())
                        perform prequestbuf
                            (Channel.updateMessage channel 100 0 (Channel.plainText "message text 2") [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 0
                                , WebDriver.ParamText "message" $ "message text 2"
                                ]
                                (object
                                    [ "response" .= Number 20
                                    ])
                            ]
                            (flip shouldBe $
                                Right ())
        it "receives button events" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.poll channel)
                            [ ExpectedRequest
                                "https://api.vk.com/method/groups.getLongPollServer"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "group_id" $ groupId
                                ]
                                (object
                                    [ "response" .= object
                                        [ "key" .= String "lpkey"
                                        , "server" .= String "https://lp.vk.com/lpadr/lpadr2"
                                        , "ts" .= String "10"
                                        ]
                                    ])
                            , ExpectedRequest
                                "https://lp.vk.com/lpadr/lpadr2"
                                [ WebDriver.ParamText "key" $ "lpkey"
                                , WebDriver.ParamText "ts" $ "10"
                                , WebDriver.ParamText "act" $ "a_check"
                                , WebDriver.ParamNum "wait" $ timeout
                                ]
                                (object
                                    [ "ts" .= String "20"
                                    , "updates" .=
                                        [ object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 10
                                                    , "text" .= String "qtext 1"
                                                    ]
                                                ]
                                            ]
                                        , object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 11
                                                    , "text" .= String "qtext 2"
                                                    , "payload" .= String "\"qdata2\""
                                                    ]
                                                ]
                                            ]
                                        , object
                                            [ "type" .= String "message_new"
                                            , "object" .= object
                                                [ "message" .= object
                                                    [ "peer_id" .= Number 100
                                                    , "conversation_message_id" .= Number 12
                                                    , "text" .= String "qtext 3"
                                                    , "payload" .= String "\"qdata \\\\ \\\" 3\""
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ])
                            ]
                            (flip shouldBe $
                                [ Channel.EventMessage 100 10 $ Channel.plainText "qtext 1"
                                , Channel.EventQuery 100 0 "" "qdata2"
                                , Channel.EventQuery 100 0 "" "qdata \\ \" 3"
                                ])
        it "sends answers to button queries" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.answerQuery channel "quid" "msg")
                            []
                            (flip shouldBe $
                                Right ())
        it "sends media" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
                            (Channel.sendMedia channel 100 "message text"
                                [ Channel.SendableMedia Channel.MediaPhoto "photo12_34"
                                , Channel.SendableMedia Channel.MediaPhoto "photo12_35_abcd"
                                , Channel.SendableMedia Channel.MediaPhoto "photo12_36_efef"
                                ])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 0
                                , WebDriver.ParamText "message" $ "message text"
                                , WebDriver.ParamText "attachment" $ "photo12_34,photo12_35_abcd,photo12_36_efef"
                                ]
                                (object
                                    [ "response" .= Number 20
                                    ])
                            ]
                            (flip shouldBe $
                                Right ())
                        perform prequestbuf
                            (Channel.sendMedia channel 100 "message text"
                                [ Channel.SendableMedia Channel.MediaPhoto "photo12_34"
                                , Channel.SendableMedia Channel.MediaPhoto "photo12_35_abcd"
                                , Channel.SendableMedia Channel.MediaSticker "sticker20"
                                , Channel.SendableMedia Channel.MediaSticker "sticker30"
                                , Channel.SendableMedia Channel.MediaPhoto "photo12_36_efef"
                                , Channel.SendableMedia Channel.MediaSticker "sticker40"
                                ])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 1
                                , WebDriver.ParamText "message" $ "message text"
                                , WebDriver.ParamText "attachment" $ "photo12_34,photo12_35_abcd"
                                ]
                                (object
                                    [ "response" .= Number 20
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 2
                                , WebDriver.ParamText "sticker_id" $ "sticker20"
                                ]
                                (object
                                    [ "response" .= Number 21
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 3
                                , WebDriver.ParamText "sticker_id" $ "sticker30"
                                ]
                                (object
                                    [ "response" .= Number 21
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 4
                                , WebDriver.ParamText "message" $ ""
                                , WebDriver.ParamText "attachment" $ "photo12_36_efef"
                                ]
                                (object
                                    [ "response" .= Number 20
                                    ])
                            , ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 5
                                , WebDriver.ParamText "sticker_id" $ "sticker40"
                                ]
                                (object
                                    [ "response" .= Number 20
                                    ])
                            ]
                            (flip shouldBe $
                                Right ())
        it "sends rich text messages as plain text" $ do
            Logger.withNullLogger $ \logger -> do
                withTestDriver $ \prequestbuf driver -> do
                    Vk.withVkChannel conf randomSeed logger driver $ \channel -> do
                        perform prequestbuf
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
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 0
                                , WebDriver.ParamText "message" $ "plain bold italic bolditalic plain under strike understrike boldunder under"
                                ]
                                (object
                                    [ "response" .= Number 20
                                    ])
                            ]
                            (flip shouldBe $
                                Right 20)
                        perform prequestbuf
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextLink "link url"
                                    ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "link "
                                    $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "bold"
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " link"
                                    $ Channel.RichTextEnd )
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextMention "345"
                                    ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "mention "
                                    $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "under"
                                    $ Channel.RichTextEnd )
                                $ Channel.RichTextEnd )
                                [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 1
                                , WebDriver.ParamText "message" $ "link bold link (link url) plain mention under (https://vk.com/id345)"
                                ]
                                (object
                                    [ "response" .= Number 21
                                    ])
                            ]
                            (flip shouldBe $
                                Right 21)
                        perform prequestbuf
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextMono "inline-code"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextCode "" "block-code"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextCode "code lang" "block-code-lang"
                                $ Channel.RichTextEnd )
                                [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 2
                                , WebDriver.ParamText "message" $ "inline-code plain block-code plain block-code-lang"
                                ]
                                (object
                                    [ "response" .= Number 22
                                    ])
                            ]
                            (flip shouldBe $
                                Right 22)
                        perform prequestbuf
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "tt \x1F914"
                                $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "\x1F914 bb"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " \x1F914\x1F914 "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "uu"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " tt"
                                $ Channel.RichTextEnd )
                                [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 3
                                , WebDriver.ParamText "message" $ "tt \x1F914\x1F914 bb \x1F914\x1F914 uu tt"
                                ]
                                (object
                                    [ "response" .= Number 23
                                    ])
                            ]
                            (flip shouldBe $
                                Right 23)
                        perform prequestbuf
                            (Channel.sendMessage channel 100
                                ( Channel.RichTextSpan (Channel.SpanStyle False False False False) "abc<>&\""
                                $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "abc<>&\""
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) "abc<>&\""
                                $ Channel.RichTextEnd )
                                [])
                            [ ExpectedRequest
                                "https://api.vk.com/method/messages.send"
                                [ WebDriver.ParamText "v" $ Vk.apiVersion
                                , WebDriver.ParamText "access_token" $ token
                                , WebDriver.ParamNum "peer_id" $ 100
                                , WebDriver.ParamNum "random_id" $ randoms !! 4
                                , WebDriver.ParamText "message" $ "abc<>&\"abc<>&\"abc<>&\""
                                ]
                                (object
                                    [ "response" .= Number 24
                                    ])
                            ]
                            (flip shouldBe $
                                Right 24)
