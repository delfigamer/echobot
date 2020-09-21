{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}


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
import Network.HTTP.Req
import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified System.IO as IO
import qualified Channel
import qualified Logger
import qualified Responder
import qualified Responder.Repeat


data Action a where
    SendMessage :: Channel.ChatId -> Channel.RichText -> [Channel.QueryButton] -> Action (Either Text.Text Channel.MessageId)
    SendMedia :: Channel.ChatId -> Text.Text -> [Channel.SendableMedia] -> Action (Either Text.Text ())
    PossessMedia :: Channel.ChatId -> Channel.ForeignMedia -> Action Channel.PossessMediaOutcome
    UpdateMessage :: Channel.ChatId -> Channel.MessageId -> Channel.RichText -> [Channel.QueryButton] -> Action (Either Text.Text ())
    AnswerQuery :: Channel.QueryId -> Text.Text -> Action (Either Text.Text ())
deriving instance Show (Action a)
deriving instance Eq (Action a)


infixr 0 :>>
data ActionExpectation = forall a . Show a => Action a :>> a
deriving instance Show ActionExpectation


newtype NakedString = NakedString String deriving (Eq)


instance Show NakedString where
    showsPrec d (NakedString x) = showParen (d > 10) $ showString x


withTestChannel
    :: (IORef (Maybe [Channel.Event]) -> IORef [ActionExpectation] -> Channel.Handle -> IO r)
    -> IO r
withTestChannel body = do
    pevents <- newIORef $ Nothing
    pexpectations <- newIORef $ []
    body pevents pexpectations $ Channel.Handle
        { Channel.poll = testPoll pevents
        , Channel.sendMessage = testSendMessage pexpectations
        , Channel.sendMedia = testSendMedia pexpectations
        , Channel.possessMedia = testPossessMedia pexpectations
        , Channel.updateMessage = testUpdateMessage pexpectations
        , Channel.answerQuery = testAnswerQuery pexpectations }


testPoll :: IORef (Maybe [Channel.Event]) -> IO [Channel.Event]
testPoll pevents = do
    Just events <- readIORef pevents
    writeIORef pevents $! Nothing
    return events


testSendMessage :: IORef [ActionExpectation] -> Channel.ChatId -> Channel.RichText -> [Channel.QueryButton] -> IO (Either Text.Text Channel.MessageId)
testSendMessage pexpectations chatId text buttons = do
    expectAction pexpectations $ SendMessage chatId text buttons


testSendMedia :: IORef [ActionExpectation] -> Channel.ChatId -> Text.Text -> [Channel.SendableMedia] -> IO (Either Text.Text ())
testSendMedia pexpectations chatId caption media = do
    expectAction pexpectations $ SendMedia chatId caption media


testPossessMedia :: IORef [ActionExpectation] -> Channel.ChatId -> Channel.ForeignMedia -> IO Channel.PossessMediaOutcome
testPossessMedia pexpectations chatId media = do
    expectAction pexpectations $ PossessMedia chatId media


testUpdateMessage :: IORef [ActionExpectation] -> Channel.ChatId -> Channel.MessageId -> Channel.RichText -> [Channel.QueryButton] -> IO (Either Text.Text ())
testUpdateMessage pexpectations chatId messageId text buttons = do
    expectAction pexpectations $ UpdateMessage chatId messageId text buttons


testAnswerQuery :: IORef [ActionExpectation] -> Channel.QueryId -> Text.Text -> IO (Either Text.Text ())
testAnswerQuery pexpectations queryId text = do
    expectAction pexpectations $ AnswerQuery queryId text


expectAction :: IORef [ActionExpectation] -> Action a -> IO a
expectAction pexpectations action = do
    expectations <- readIORef pexpectations
    case expectations of
        top@(expaction :>> _):rest -> do
            (NakedString $ show action) `shouldBe` (NakedString $ show expaction)
            writeIORef pexpectations $! rest
            return $ matchAction action top
        [] -> do
            expectationFailure $ "Unexpected action: " ++ show action
            undefined


matchAction :: Action a -> ActionExpectation -> a
matchAction (SendMessage {}) (SendMessage {} :>> r) = r
matchAction (SendMedia {}) (SendMedia {} :>> r) = r
matchAction (PossessMedia {}) (PossessMedia {} :>> r) = r
matchAction (UpdateMessage {}) (UpdateMessage {} :>> r) = r
matchAction (AnswerQuery {}) (AnswerQuery {} :>> r) = r
matchAction _ _ = undefined


oneWork
    :: IORef (Maybe [Channel.Event])
    -> IORef [ActionExpectation]
    -> Responder.Handle
    -> [Channel.Event]
    -> [ActionExpectation]
    -> IO ()
oneWork pevents pexpectations responder events actions = do
    writeIORef pevents $! Just $! events
    writeIORef pexpectations $! actions
    Responder.work responder
    readIORef pevents `shouldReturn` Nothing
    readIORef pexpectations >>= flip shouldSatisfy null


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
        let mediaUnknownTypeMsg s = "unknown media type " <> s
        let mediaUnsupportedMsg = "unsupported media"
        let mediaInternalErrorMsg = "media internal error"
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
                , Responder.Repeat.cMediaUnknownTypeMsg = mediaUnknownTypeMsg "%1"
                , Responder.Repeat.cMediaUnsupportedMsg = mediaUnsupportedMsg
                , Responder.Repeat.cMediaInternalErrorMsg = mediaInternalErrorMsg
                , Responder.Repeat.cMaxMultiplier = 4 }
        let inspectButtons =
                [ Channel.QueryButton "1" "r1"
                , Channel.QueryButton "2" "r2"
                , Channel.QueryButton "3" "r3"
                , Channel.QueryButton "4" "r4"
                ]
        let foreignMedia t n = Channel.ForeignMedia t ("id" <> n) ("url" <> n)
        let possessMediaSuccess t n = Channel.PossessMediaSuccess $ Channel.SendableMedia t n
        it "repeats messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 105 600 (Channel.plainText "some text")
                            ]
                            [ SendMessage 105 (Channel.plainText "some text") [] :>> Right 603
                            , SendMessage 105 (Channel.plainText "some text") [] :>> Right 604
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 500 (Channel.plainText "mytext 500")
                            , Channel.EventMessage 100 501 (Channel.plainText "mytext 501")
                            ]
                            [ SendMessage 100 (Channel.plainText "mytext 500") [] :>> Right 502
                            , SendMessage 100 (Channel.plainText "mytext 500") [] :>> Right 503
                            , SendMessage 100 (Channel.plainText "mytext 501") [] :>> Right 504
                            , SendMessage 100 (Channel.plainText "mytext 501") [] :>> Right 505
                            ]
        it "detects unknown commands" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 105 601 (Channel.plainText "/unknown-command")
                            ]
                            [ SendMessage 105 (Channel.plainText unknownCmdMsg) [] :>> Right 605
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 102 510 (Channel.plainText "/unknown-command")
                            , Channel.EventMessage 102 511 (Channel.plainText "other text")
                            ]
                            [ SendMessage 102 (Channel.plainText unknownCmdMsg) [] :>> Right 512
                            , SendMessage 102 (Channel.plainText "other text") [] :>> Right 513
                            , SendMessage 102 (Channel.plainText "other text") [] :>> Right 514
                            ]
        it "allows to change the multiplier" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 105 601 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 105 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 605
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventQuery 105 605 "qid1" "r3"
                            ]
                            [ AnswerQuery "qid1" "" :>> Right ()
                            , UpdateMessage 105 605 (Channel.plainText (multiplierSetMsg "3")) [] :>> Right ()
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 105 607 (Channel.plainText "message in 105")
                            ]
                            [ SendMessage 105 (Channel.plainText "message in 105") [] :>> Right 608
                            , SendMessage 105 (Channel.plainText "message in 105") [] :>> Right 609
                            , SendMessage 105 (Channel.plainText "message in 105") [] :>> Right 610
                            ]
        it "keeps individual multipliers for each chat" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 11 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 200 21 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 300 31 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 12
                            , SendMessage 200 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 22
                            , SendMessage 300 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 32
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventQuery 300 32 "qid3" "r3"
                            , Channel.EventQuery 100 12 "qid1" "r1"
                            , Channel.EventQuery 200 22 "qid2" "r2"
                            ]
                            [ AnswerQuery "qid3" "" :>> Right ()
                            , UpdateMessage 300 32 (Channel.plainText (multiplierSetMsg "3")) [] :>> Right ()
                            , AnswerQuery "qid1" "" :>> Right ()
                            , UpdateMessage 100 12 (Channel.plainText (multiplierSetMsg "1")) [] :>> Right ()
                            , AnswerQuery "qid2" "" :>> Right ()
                            , UpdateMessage 200 22 (Channel.plainText (multiplierSetMsg "2")) [] :>> Right ()
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 200 23 (Channel.plainText "message in 200")
                            , Channel.EventMessage 300 33 (Channel.plainText "message in 300")
                            , Channel.EventMessage 100 13 (Channel.plainText "message in 100")
                            , Channel.EventMessage 500 53 (Channel.plainText "message in 500")
                            ]
                            [ SendMessage 200 (Channel.plainText "message in 200") [] :>> Right 24
                            , SendMessage 200 (Channel.plainText "message in 200") [] :>> Right 25
                            , SendMessage 300 (Channel.plainText "message in 300") [] :>> Right 34
                            , SendMessage 300 (Channel.plainText "message in 300") [] :>> Right 35
                            , SendMessage 300 (Channel.plainText "message in 300") [] :>> Right 36
                            , SendMessage 100 (Channel.plainText "message in 100") [] :>> Right 14
                            , SendMessage 500 (Channel.plainText "message in 500") [] :>> Right 54
                            , SendMessage 500 (Channel.plainText "message in 500") [] :>> Right 55
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 100 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 500 100 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 200 100 (Channel.plainText inspectMultiplierCmd)
                            , Channel.EventMessage 300 100 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "1")) inspectButtons :>> Right 101
                            , SendMessage 500 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 101
                            , SendMessage 200 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 101
                            , SendMessage 300 (Channel.plainText (inspectMultiplierMsg "3")) inspectButtons :>> Right 101
                            ]
        it "starts itself" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 11 (Channel.plainText startCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (startMsg "2")) [] :>> Right 12
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 13 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 14
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventQuery 100 14 "qid1" "r1"
                            ]
                            [ AnswerQuery "qid1" "" :>> Right ()
                            , UpdateMessage 100 14 (Channel.plainText (multiplierSetMsg "1")) [] :>> Right ()
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 15 (Channel.plainText startCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (startMsg "1")) [] :>> Right 16
                            ]
        it "describes itself" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 11 (Channel.plainText describeCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (describeMsg "2")) [] :>> Right 12
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 13 (Channel.plainText inspectMultiplierCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (inspectMultiplierMsg "2")) inspectButtons :>> Right 14
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventQuery 100 14 "qid1" "r4"
                            ]
                            [ AnswerQuery "qid1" "" :>> Right ()
                            , UpdateMessage 100 14 (Channel.plainText (multiplierSetMsg "4")) [] :>> Right ()
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 15 (Channel.plainText describeCmd)
                            ]
                            [ SendMessage 100 (Channel.plainText (describeMsg "4")) [] :>> Right 16
                            ]
        it "repeats media" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
                    Responder.Repeat.withRepeatResponder config logger channel $ \responder -> do
                        oneWork pevents pexpectations responder
                            [ Channel.EventMedia 100 ""
                                [ foreignMedia Channel.MediaPhoto "photo 100"
                                ]
                            , Channel.EventMedia 100 "caption 1"
                                [ foreignMedia Channel.MediaPhoto "photo 100 b"
                                , foreignMedia Channel.MediaVideo "video 100"
                                ]
                            ]
                            [ PossessMedia 100 (foreignMedia Channel.MediaPhoto "photo 100") :>> possessMediaSuccess Channel.MediaPhoto "photo 100"
                            , SendMedia 100 "" [Channel.SendableMedia Channel.MediaPhoto "photo 100"] :>> Right ()
                            , SendMedia 100 "" [Channel.SendableMedia Channel.MediaPhoto "photo 100"] :>> Right ()
                            , PossessMedia 100 (foreignMedia Channel.MediaPhoto "photo 100 b") :>> possessMediaSuccess Channel.MediaPhoto "photo 100 b"
                            , PossessMedia 100 (foreignMedia Channel.MediaVideo "video 100") :>> possessMediaSuccess Channel.MediaVideo "video 100"
                            , SendMedia 100 "caption 1" [Channel.SendableMedia Channel.MediaPhoto "photo 100 b", Channel.SendableMedia Channel.MediaVideo "video 100"] :>> Right ()
                            , SendMedia 100 "caption 1" [Channel.SendableMedia Channel.MediaPhoto "photo 100 b", Channel.SendableMedia Channel.MediaVideo "video 100"] :>> Right ()
                            ]
                        oneWork pevents pexpectations responder
                            [ Channel.EventMedia 100 "caption 3"
                                [ foreignMedia Channel.MediaAudio "audio 100"
                                , foreignMedia Channel.MediaAnimation "anim 100"
                                , foreignMedia Channel.MediaUnknown "unknown 100"
                                , foreignMedia Channel.MediaVoice "voice 100"
                                , foreignMedia Channel.MediaDocument "doc 100"
                                , foreignMedia Channel.MediaSticker "sticker 100"
                                , foreignMedia Channel.MediaAnimation "anim 100 b"
                                , foreignMedia Channel.MediaUnknown "unknown 100 b"
                                , foreignMedia Channel.MediaUnknown "unknown 100 c"
                                , foreignMedia Channel.MediaAudio "audio 100 b"
                                ]
                            ]
                            [ PossessMedia 100 (foreignMedia Channel.MediaAudio "audio 100") :>> possessMediaSuccess Channel.MediaAudio "audio 100"
                            , PossessMedia 100 (foreignMedia Channel.MediaAnimation "anim 100") :>> Channel.PossessMediaUnsupported
                            , PossessMedia 100 (foreignMedia Channel.MediaUnknown "unknown 100") :>> Channel.PossessMediaUnknownType "unknown b"
                            , PossessMedia 100 (foreignMedia Channel.MediaVoice "voice 100") :>> Channel.PossessMediaInternalError
                            , PossessMedia 100 (foreignMedia Channel.MediaDocument "doc 100") :>> Channel.PossessMediaInternalError
                            , PossessMedia 100 (foreignMedia Channel.MediaSticker "sticker 100") :>> possessMediaSuccess Channel.MediaSticker "sticker 100"
                            , PossessMedia 100 (foreignMedia Channel.MediaAnimation "anim 100 b") :>> Channel.PossessMediaUnsupported
                            , PossessMedia 100 (foreignMedia Channel.MediaUnknown "unknown 100 b") :>> Channel.PossessMediaUnknownType "unknown b"
                            , PossessMedia 100 (foreignMedia Channel.MediaUnknown "unknown 100 c") :>> Channel.PossessMediaUnknownType "unknown a"
                            , PossessMedia 100 (foreignMedia Channel.MediaAudio "audio 100 b") :>> possessMediaSuccess Channel.MediaAudio "audio 100 b"
                            , SendMedia 100 "caption 3"
                                [ Channel.SendableMedia Channel.MediaAudio "audio 100"
                                , Channel.SendableMedia Channel.MediaSticker "sticker 100"
                                , Channel.SendableMedia Channel.MediaAudio "audio 100 b"
                                ]
                                :>> Right ()
                            , SendMedia 100 "caption 3"
                                [ Channel.SendableMedia Channel.MediaAudio "audio 100"
                                , Channel.SendableMedia Channel.MediaSticker "sticker 100"
                                , Channel.SendableMedia Channel.MediaAudio "audio 100 b"
                                ]
                                :>> Right ()
                            , SendMessage 100 (Channel.plainText (mediaUnknownTypeMsg "unknown a")) [] :>> Right 11
                            , SendMessage 100 (Channel.plainText (mediaUnknownTypeMsg "unknown b")) [] :>> Right 12
                            , SendMessage 100 (Channel.plainText mediaUnsupportedMsg) [] :>> Right 13
                            , SendMessage 100 (Channel.plainText mediaInternalErrorMsg) [] :>> Right 14
                            ]
        it "repeats rich text messages" $ do
            Logger.withNullLogger $ \logger -> do
                withTestChannel $ \pevents pexpectations channel -> do
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
                        oneWork pevents pexpectations responder
                            [ Channel.EventMessage 100 11 message1
                            , Channel.EventMessage 100 12 message2
                            , Channel.EventMessage 100 13 message3
                            , Channel.EventMessage 100 14 message4
                            ]
                            [ SendMessage 100 message1 [] :>> Right 20
                            , SendMessage 100 message1 [] :>> Right 21
                            , SendMessage 100 message2 [] :>> Right 22
                            , SendMessage 100 message2 [] :>> Right 23
                            , SendMessage 100 message3 [] :>> Right 24
                            , SendMessage 100 message3 [] :>> Right 25
                            , SendMessage 100 message4 [] :>> Right 26
                            , SendMessage 100 message4 [] :>> Right 27
                            ]
