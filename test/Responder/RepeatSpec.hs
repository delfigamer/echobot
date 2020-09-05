{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Responder.RepeatSpec
    ( spec
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
import qualified Data.Text.Encoding as TextEncoding
import qualified System.IO as IO
import qualified Channel
import qualified Logger
import qualified Responder
import qualified Responder.Repeat


data ExpectedAction
    = SendMessage Channel.ChatId Text [Channel.QueryButton] (Either Text Channel.MessageId)
    | SendSticker Channel.ChatId Channel.StickerName (Either Text ())
    | UpdateMessage Channel.ChatId Channel.MessageId Text [Channel.QueryButton] (Either Text ())
    | AnswerQuery Channel.QueryId Text (Either Text ())
    deriving (Show, Eq)


withTestChannel
    :: (IORef (Maybe [Channel.Event]) -> IORef [ExpectedAction] -> Channel.Handle -> IO r)
    -> IO r
withTestChannel body = do
    pevents <- newIORef $ Nothing
    pactions <- newIORef $ []
    body pevents pactions $ Channel.Handle
        { Channel.poll = testPoll pevents
        , Channel.sendMessage = testSendMessage pactions
        , Channel.sendSticker = testSendSticker pactions
        , Channel.updateMessage = testUpdateMessage pactions
        , Channel.answerQuery = testAnswerQuery pactions }


testPoll :: IORef (Maybe [Channel.Event]) -> IO [Channel.Event]
testPoll pevents = do
    Just events <- readIORef pevents
    writeIORef pevents $! Nothing
    return events


testSendMessage :: IORef [ExpectedAction] -> Channel.ChatId -> Text -> [Channel.QueryButton] -> IO (Either Text Channel.MessageId)
testSendMessage pactions chatId text buttons = do
    SendMessage expchatId exptext expbuttons result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (chatId, text, buttons) `shouldBe` (expchatId, exptext, expbuttons)
    return $ result


testSendSticker :: IORef [ExpectedAction] -> Channel.ChatId -> Channel.StickerName -> IO (Either Text ())
testSendSticker pactions chatId sticker = do
    SendSticker expchatId expsticker result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (chatId, sticker) `shouldBe` (expchatId, expsticker)
    return $ result


testUpdateMessage :: IORef [ExpectedAction] -> Channel.ChatId -> Channel.MessageId -> Text -> [Channel.QueryButton] -> IO (Either Text ())
testUpdateMessage pactions chatId messageId text buttons = do
    UpdateMessage expchatId expmessageId exptext expbuttons result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (chatId, messageId, text, buttons) `shouldBe` (expchatId, expmessageId, exptext, expbuttons)
    return $ result


testAnswerQuery :: IORef [ExpectedAction] -> Channel.QueryId -> Text -> IO (Either Text ())
testAnswerQuery pactions queryId text = do
    AnswerQuery expqueryId exptext result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (expqueryId, exptext) `shouldBe` (expqueryId, exptext)
    return $ result


oneWork
    :: IORef (Maybe [Channel.Event])
    -> IORef [ExpectedAction]
    -> Responder.Handle
    -> [Channel.Event]
    -> [ExpectedAction]
    -> IO ()
oneWork pevents pactions responder events actions = do
    writeIORef pevents $! Just $! events
    writeIORef pactions $! actions
    Responder.work responder
    readIORef pevents `shouldReturn` Nothing
    readIORef pactions `shouldReturn` []


spec :: Spec
spec = do
    describe "Responder.Repeat" $ do
        let unknownCmdMsg = "unknown command"
        let describeCmd = "/help"
        let describeMsg s = "this is help " <> s
        let inspectMultiplierCmd = "/repeat"
        let inspectMultiplierMsg s = "repeat is " <> s
        let multiplierSetMsg s = "repeat set to " <> s
        let config = Responder.Repeat.Config
                { Responder.Repeat.cDefaultMultiplier = 2
                , Responder.Repeat.cUnknownCommandMsg = unknownCmdMsg
                , Responder.Repeat.cDescribeCmd = describeCmd
                , Responder.Repeat.cDescribeMsg = describeMsg "%1"
                , Responder.Repeat.cInspectMultiplierCmd = inspectMultiplierCmd
                , Responder.Repeat.cInspectMultiplierMsg = inspectMultiplierMsg "%1"
                , Responder.Repeat.cMultiplierSetMsg = multiplierSetMsg "%1"
                , Responder.Repeat.cMaxMultiplier = 4 }
        let inspectButtons =
                [ Channel.QueryButton "1" "r1"
                , Channel.QueryButton "2" "r2"
                , Channel.QueryButton "3" "r3"
                , Channel.QueryButton "4" "r4"
                ]
        it "repeats messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 105 600 "some text"
                            ]
                            [ SendMessage 105 "some text" [] (Right 603)
                            , SendMessage 105 "some text" [] (Right 604)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 500 "mytext 500"
                            , Channel.EventMessage 100 501 "mytext 501"
                            ]
                            [ SendMessage 100 "mytext 500" [] (Right 502)
                            , SendMessage 100 "mytext 500" [] (Right 503)
                            , SendMessage 100 "mytext 501" [] (Right 504)
                            , SendMessage 100 "mytext 501" [] (Right 505)
                            ]
        it "detects unknown commands" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 105 601 "/unknown-command"
                            ]
                            [ SendMessage 105 unknownCmdMsg [] (Right 605)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 102 510 "/unknown-command"
                            , Channel.EventMessage 102 511 "other text"
                            ]
                            [ SendMessage 102 unknownCmdMsg [] (Right 512)
                            , SendMessage 102 "other text" [] (Right 513)
                            , SendMessage 102 "other text" [] (Right 514)
                            ]
        it "allows to change the multiplier" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 105 601 inspectMultiplierCmd
                            ]
                            [ SendMessage 105 (inspectMultiplierMsg "2") inspectButtons (Right 605)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventQuery 105 605 "qid1" "r3"
                            ]
                            [ AnswerQuery "qid1" "" (Right ())
                            , UpdateMessage 105 605 (multiplierSetMsg "3") [] (Right ())
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 105 607 "message in 105"
                            ]
                            [ SendMessage 105 "message in 105" [] (Right 608)
                            , SendMessage 105 "message in 105" [] (Right 609)
                            , SendMessage 105 "message in 105" [] (Right 610)
                            ]
        it "keeps individual multipliers for each chat" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 11 inspectMultiplierCmd
                            , Channel.EventMessage 200 21 inspectMultiplierCmd
                            , Channel.EventMessage 300 31 inspectMultiplierCmd
                            ]
                            [ SendMessage 100 (inspectMultiplierMsg "2") inspectButtons (Right 12)
                            , SendMessage 200 (inspectMultiplierMsg "2") inspectButtons (Right 22)
                            , SendMessage 300 (inspectMultiplierMsg "2") inspectButtons (Right 32)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventQuery 300 32 "qid3" "r3"
                            , Channel.EventQuery 100 12 "qid1" "r1"
                            , Channel.EventQuery 200 22 "qid2" "r2"
                            ]
                            [ AnswerQuery "qid3" "" (Right ())
                            , UpdateMessage 300 32 (multiplierSetMsg "3") [] (Right ())
                            , AnswerQuery "qid1" "" (Right ())
                            , UpdateMessage 100 12 (multiplierSetMsg "1") [] (Right ())
                            , AnswerQuery "qid2" "" (Right ())
                            , UpdateMessage 200 22 (multiplierSetMsg "2") [] (Right ())
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 200 23 "message in 200"
                            , Channel.EventMessage 300 33 "message in 300"
                            , Channel.EventMessage 100 13 "message in 100"
                            , Channel.EventMessage 500 53 "message in 500"
                            ]
                            [ SendMessage 200 "message in 200" [] (Right 24)
                            , SendMessage 200 "message in 200" [] (Right 25)
                            , SendMessage 300 "message in 300" [] (Right 34)
                            , SendMessage 300 "message in 300" [] (Right 35)
                            , SendMessage 300 "message in 300" [] (Right 36)
                            , SendMessage 100 "message in 100" [] (Right 14)
                            , SendMessage 500 "message in 500" [] (Right 54)
                            , SendMessage 500 "message in 500" [] (Right 55)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 100 inspectMultiplierCmd
                            , Channel.EventMessage 500 100 inspectMultiplierCmd
                            , Channel.EventMessage 200 100 inspectMultiplierCmd
                            , Channel.EventMessage 300 100 inspectMultiplierCmd
                            ]
                            [ SendMessage 100 (inspectMultiplierMsg "1") inspectButtons (Right 101)
                            , SendMessage 500 (inspectMultiplierMsg "2") inspectButtons (Right 101)
                            , SendMessage 200 (inspectMultiplierMsg "2") inspectButtons (Right 101)
                            , SendMessage 300 (inspectMultiplierMsg "3") inspectButtons (Right 101)
                            ]
        it "describes itself" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 11 describeCmd
                            ]
                            [ SendMessage 100 (describeMsg "2") [] (Right 12)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 13 inspectMultiplierCmd
                            ]
                            [ SendMessage 100 (inspectMultiplierMsg "2") inspectButtons (Right 14)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventQuery 100 14 "qid1" "r4"
                            ]
                            [ AnswerQuery "qid1" "" (Right ())
                            , UpdateMessage 100 14 (multiplierSetMsg "4") [] (Right ())
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 15 describeCmd
                            ]
                            [ SendMessage 100 (describeMsg "4") [] (Right 16)
                            ]
