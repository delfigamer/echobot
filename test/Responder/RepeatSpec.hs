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
    = SendMessage Channel.ChatId Channel.RichText [Channel.QueryButton] (Either Text Channel.MessageId)
    | SendSticker Channel.ChatId Channel.FileId (Either Text ())
    | SendMedia Channel.ChatId Text Channel.Media (Either Text ())
    | SendMediaGroup Channel.ChatId Channel.MediaGroup (Either Text ())
    | UpdateMessage Channel.ChatId Channel.MessageId Channel.RichText [Channel.QueryButton] (Either Text ())
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
        , Channel.sendMedia = testSendMedia pactions
        , Channel.sendMediaGroup = testSendMediaGroup pactions
        , Channel.updateMessage = testUpdateMessage pactions
        , Channel.answerQuery = testAnswerQuery pactions }


testPoll :: IORef (Maybe [Channel.Event]) -> IO [Channel.Event]
testPoll pevents = do
    Just events <- readIORef pevents
    writeIORef pevents $! Nothing
    return events


testSendMessage :: IORef [ExpectedAction] -> Channel.ChatId -> Channel.RichText -> [Channel.QueryButton] -> IO (Either Text Channel.MessageId)
testSendMessage pactions chatId text buttons = do
    SendMessage expchatId exptext expbuttons result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (chatId, text, buttons) `shouldBe` (expchatId, exptext, expbuttons)
    return $ result


testSendSticker :: IORef [ExpectedAction] -> Channel.ChatId -> Channel.FileId -> IO (Either Text ())
testSendSticker pactions chatId sticker = do
    SendSticker expchatId expsticker result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (chatId, sticker) `shouldBe` (expchatId, expsticker)
    return $ result


testSendMedia :: IORef [ExpectedAction] -> Channel.ChatId -> Text -> Channel.Media -> IO (Either Text ())
testSendMedia pactions chatId caption media = do
    SendMedia expchatId expcaption expmedia result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (chatId, caption, media) `shouldBe` (expchatId, expcaption, expmedia)
    return $ result


testSendMediaGroup :: IORef [ExpectedAction] -> Channel.ChatId -> Channel.MediaGroup -> IO (Either Text ())
testSendMediaGroup pactions chatId group = do
    SendMediaGroup expchatId expgroup result:rest <- readIORef pactions
    writeIORef pactions $! rest
    (chatId, group) `shouldBe` (expchatId, expgroup)
    return $ result


testUpdateMessage :: IORef [ExpectedAction] -> Channel.ChatId -> Channel.MessageId -> Channel.RichText -> [Channel.QueryButton] -> IO (Either Text ())
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
        let startCmd = "/start"
        let startMsg s = "i am started " <> s
        let describeCmd = "/help"
        let describeMsg s = "this is help " <> s
        let inspectMultiplierCmd = "/repeat"
        let inspectMultiplierMsg s = "repeat is " <> s
        let multiplierSetMsg s = "repeat set to " <> s
        let config = Responder.Repeat.Config
                { Responder.Repeat.cDefaultMultiplier = 2
                , Responder.Repeat.cUnknownCommandMsg = unknownCmdMsg
                , Responder.Repeat.cStartCmd = startCmd
                , Responder.Repeat.cStartMsg = startMsg "%1"
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
                            [ Channel.EventMessage 105 600 (Channel.plainText "some text")
                            ]
                            [ SendMessage 105 (Channel.plainText "some text") [] (Right 603)
                            , SendMessage 105 (Channel.plainText "some text") [] (Right 604)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 500 (Channel.plainText "mytext 500")
                            , Channel.EventMessage 100 501 (Channel.plainText "mytext 501")
                            ]
                            [ SendMessage 100 (Channel.plainText "mytext 500") [] (Right 502)
                            , SendMessage 100 (Channel.plainText "mytext 500") [] (Right 503)
                            , SendMessage 100 (Channel.plainText "mytext 501") [] (Right 504)
                            , SendMessage 100 (Channel.plainText "mytext 501") [] (Right 505)
                            ]
        it "detects unknown commands" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 105 601 (Channel.plainText "/unknown-command")
                            ]
                            [ SendMessage 105 (Channel.plainText unknownCmdMsg) [] (Right 605)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 102 510 (Channel.plainText "/unknown-command")
                            , Channel.EventMessage 102 511 (Channel.plainText "other text")
                            ]
                            [ SendMessage 102 (Channel.plainText unknownCmdMsg) [] (Right 512)
                            , SendMessage 102 (Channel.plainText "other text") [] (Right 513)
                            , SendMessage 102 (Channel.plainText "other text") [] (Right 514)
                            ]
        it "allows to change the multiplier" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 105 601 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 105 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 605)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventQuery 105 605 "qid1" "r3"
                            ]
                            [ AnswerQuery "qid1" "" (Right ())
                            , UpdateMessage 105 605 (Channel.plainText (multiplierSetMsg "3")) [] (Right ())
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 105 607 (Channel.plainText "message in 105")
                            ]
                            [ SendMessage 105 (Channel.plainText "message in 105") [] (Right 608)
                            , SendMessage 105 (Channel.plainText "message in 105") [] (Right 609)
                            , SendMessage 105 (Channel.plainText "message in 105") [] (Right 610)
                            ]
        it "keeps individual multipliers for each chat" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 11 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 200 21 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 300 31 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 12)
                            , SendMessage 200 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 22)
                            , SendMessage 300 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 32)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventQuery 300 32 "qid3" "r3"
                            , Channel.EventQuery 100 12 "qid1" "r1"
                            , Channel.EventQuery 200 22 "qid2" "r2"
                            ]
                            [ AnswerQuery "qid3" "" (Right ())
                            , UpdateMessage 300 32 (Channel.plainText (multiplierSetMsg "3")) [] (Right ())
                            , AnswerQuery "qid1" "" (Right ())
                            , UpdateMessage 100 12 (Channel.plainText (multiplierSetMsg "1")) [] (Right ())
                            , AnswerQuery "qid2" "" (Right ())
                            , UpdateMessage 200 22 (Channel.plainText (multiplierSetMsg "2")) [] (Right ())
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 200 23 (Channel.plainText "message in 200")
                            , Channel.EventMessage 300 33 (Channel.plainText "message in 300")
                            , Channel.EventMessage 100 13 (Channel.plainText "message in 100")
                            , Channel.EventMessage 500 53 (Channel.plainText "message in 500")
                            ]
                            [ SendMessage 200 (Channel.plainText "message in 200") [] (Right 24)
                            , SendMessage 200 (Channel.plainText "message in 200") [] (Right 25)
                            , SendMessage 300 (Channel.plainText "message in 300") [] (Right 34)
                            , SendMessage 300 (Channel.plainText "message in 300") [] (Right 35)
                            , SendMessage 300 (Channel.plainText "message in 300") [] (Right 36)
                            , SendMessage 100 (Channel.plainText "message in 100") [] (Right 14)
                            , SendMessage 500 (Channel.plainText "message in 500") [] (Right 54)
                            , SendMessage 500 (Channel.plainText "message in 500") [] (Right 55)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 100 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 500 100 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 200 100 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 300 100 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "1")) inspectButtons (Right 101)
                            , SendMessage 500 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 101)
                            , SendMessage 200 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 101)
                            , SendMessage 300 (Channel.plainText (inspectMultiplierMsg "3")) inspectButtons (Right 101)
                            ]
        it "starts itself" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 11 (Channel.plainText startCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (startMsg "2")) [] (Right 12)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 13 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 14)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventQuery 100 14 "qid1" "r1"
                            ]
                            [ AnswerQuery "qid1" "" (Right ())
                            , UpdateMessage 100 14 (Channel.plainText (multiplierSetMsg "1")) [] (Right ())
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 15 (Channel.plainText startCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (startMsg "1")) [] (Right 16)
                            ]
        it "describes itself" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 11 (Channel.plainText describeCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (describeMsg "2")) [] (Right 12)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 13 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons (Right 14)
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventQuery 100 14 "qid1" "r4"
                            ]
                            [ AnswerQuery "qid1" "" (Right ())
                            , UpdateMessage 100 14 (Channel.plainText (multiplierSetMsg "4")) [] (Right ())
                            ]
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 15 (Channel.plainText describeCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (describeMsg "4")) [] (Right 16)
                            ]
        it "repeats stickers" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventSticker 100 "sticker 100"
                            , Channel.EventSticker 200 "sticker 200"
                            ]
                            [ SendSticker 100 "sticker 100" (Right ())
                            , SendSticker 100 "sticker 100" (Right ())
                            , SendSticker 200 "sticker 200" (Right ())
                            , SendSticker 200 "sticker 200" (Right ())
                            ]
        it "repeats media" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pactions responder
                            [ Channel.EventMedia 100 "" $ Channel.MediaPhoto "photo 100"
                            , Channel.EventMedia 100 "caption 1" $ Channel.MediaPhoto "photo 100 b"
                            , Channel.EventMedia 100 "caption 2" $ Channel.MediaVideo "video 100"
                            , Channel.EventMedia 100 "caption 3" $ Channel.MediaAudio "audio 100"
                            , Channel.EventMedia 100 "caption 4" $ Channel.MediaAnimation "anim 100"
                            , Channel.EventMedia 100 "caption 5" $ Channel.MediaVoice "voice 100"
                            , Channel.EventMedia 100 "caption 6" $ Channel.MediaDocument "doc 100"
                            ]
                            [ SendMedia 100 "" (Channel.MediaPhoto "photo 100") (Right ())
                            , SendMedia 100 "" (Channel.MediaPhoto "photo 100") (Right ())
                            , SendMedia 100 "caption 1" (Channel.MediaPhoto "photo 100 b") (Right ())
                            , SendMedia 100 "caption 1" (Channel.MediaPhoto "photo 100 b") (Right ())
                            , SendMedia 100 "caption 2" (Channel.MediaVideo "video 100") (Right ())
                            , SendMedia 100 "caption 2" (Channel.MediaVideo "video 100") (Right ())
                            , SendMedia 100 "caption 3" (Channel.MediaAudio "audio 100") (Right ())
                            , SendMedia 100 "caption 3" (Channel.MediaAudio "audio 100") (Right ())
                            , SendMedia 100 "caption 4" (Channel.MediaAnimation "anim 100") (Right ())
                            , SendMedia 100 "caption 4" (Channel.MediaAnimation "anim 100") (Right ())
                            , SendMedia 100 "caption 5" (Channel.MediaVoice "voice 100") (Right ())
                            , SendMedia 100 "caption 5" (Channel.MediaVoice "voice 100") (Right ())
                            , SendMedia 100 "caption 6" (Channel.MediaDocument "doc 100") (Right ())
                            , SendMedia 100 "caption 6" (Channel.MediaDocument "doc 100") (Right ())
                            ]
        it "repeats rich text messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pactions channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        let message1 =
                                  Channel.RichTextSpan (Channel.SpanStyle False False False False) "plain "
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
                        let message2 =
                                  Channel.RichTextLink "link url"
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
                        let message3 =
                                  Channel.RichTextMono "inline-code"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextCode "" "block-code"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " plain "
                                $ Channel.RichTextCode "code lang" "block-code-lang"
                                $ Channel.RichTextEnd
                        let message4 =
                                  Channel.RichTextSpan (Channel.SpanStyle False False False False) "tt \x1F914"
                                $ Channel.RichTextSpan (Channel.SpanStyle True False False False) "\x1F914 bb"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " \x1F914\x1F914 "
                                $ Channel.RichTextSpan (Channel.SpanStyle False False True False) "uu"
                                $ Channel.RichTextSpan (Channel.SpanStyle False False False False) " tt"
                                $ Channel.RichTextEnd
                        oneWork pevents pactions responder
                            [ Channel.EventMessage 100 11 message1
                            , Channel.EventMessage 100 12 message2
                            , Channel.EventMessage 100 13 message3
                            , Channel.EventMessage 100 14 message4
                            ]
                            [ SendMessage 100 message1 [] (Right 20)
                            , SendMessage 100 message1 [] (Right 21)
                            , SendMessage 100 message2 [] (Right 22)
                            , SendMessage 100 message2 [] (Right 23)
                            , SendMessage 100 message3 [] (Right 24)
                            , SendMessage 100 message3 [] (Right 25)
                            , SendMessage 100 message4 [] (Right 26)
                            , SendMessage 100 message4 [] (Right 27)
                            ]
