{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Channel.Test
    ( Action(..)
    , ActionExpectation
    , (|>>)
    , withTestChannel
    ) where

import qualified Channel
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

data Action a where
    SendMessage
        :: Channel.ChatId
        -> Channel.RichText
        -> [Channel.QueryButton]
        -> Action (Either Text.Text Channel.MessageId)
    SendMedia
        :: Channel.ChatId
        -> Text.Text
        -> [Channel.SendableMedia]
        -> Action (Either Text.Text ())
    PossessMedia
        :: Channel.ChatId
        -> Channel.ForeignMedia
        -> Action Channel.PossessMediaOutcome
    UpdateMessage
        :: Channel.ChatId
        -> Channel.MessageId
        -> Channel.RichText
        -> [Channel.QueryButton]
        -> Action (Either Text.Text ())
    AnswerQuery :: Channel.QueryId -> Text.Text -> Action (Either Text.Text ())

deriving instance Show (Action a)

deriving instance Eq (Action a)

data ActionExpectation where
    ActionExpectation
        :: Show a
        => Action a
        -> a
        -> (forall t u. (HasCallStack =>
                             t -> u) -> (t -> u))
        -> ActionExpectation

instance Show ActionExpectation where
    showsPrec d (ActionExpectation action result _) =
        showParen (d > 0) $
        showsPrec 1 action . showString " |>> " . showsPrec 1 result

infixr 0 |>>

(|>>) :: (Show a, HasCallStack) => Action a -> a -> ActionExpectation
action |>> result = ActionExpectation action result id

data AnAction =
    forall a. AnAction (Action a)

instance Show AnAction where
    showsPrec d (AnAction x) = showsPrec d x

instance Eq AnAction where
    AnAction a@SendMessage {} == AnAction b@SendMessage {} = a == b
    AnAction a@SendMedia {} == AnAction b@SendMedia {} = a == b
    AnAction a@PossessMedia {} == AnAction b@PossessMedia {} = a == b
    AnAction a@UpdateMessage {} == AnAction b@UpdateMessage {} = a == b
    AnAction a@AnswerQuery {} == AnAction b@AnswerQuery {} = a == b
    _ == _ = False

withTestChannel ::
       (IORef (Maybe [Channel.Event]) -> IORef [ActionExpectation] -> Channel.Handle -> IO r)
    -> IO r
withTestChannel body = do
    pevents <- newIORef $ Nothing
    pexpectations <- newIORef $ []
    body pevents pexpectations $
        Channel.Handle
            { Channel.poll = testPoll pevents
            , Channel.sendMessage = testSendMessage pexpectations
            , Channel.sendMedia = testSendMedia pexpectations
            , Channel.possessMedia = testPossessMedia pexpectations
            , Channel.updateMessage = testUpdateMessage pexpectations
            , Channel.answerQuery = testAnswerQuery pexpectations
            }

testPoll :: IORef (Maybe [Channel.Event]) -> IO [Channel.Event]
testPoll pevents = do
    Just events <- readIORef pevents
    writeIORef pevents $ Nothing
    return events

testSendMessage ::
       IORef [ActionExpectation]
    -> Channel.ChatId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text.Text Channel.MessageId)
testSendMessage pexpectations chatId text buttons = do
    expectAction pexpectations $ SendMessage chatId text buttons

testSendMedia ::
       IORef [ActionExpectation]
    -> Channel.ChatId
    -> Text.Text
    -> [Channel.SendableMedia]
    -> IO (Either Text.Text ())
testSendMedia pexpectations chatId caption media = do
    expectAction pexpectations $ SendMedia chatId caption media

testPossessMedia ::
       IORef [ActionExpectation]
    -> Channel.ChatId
    -> Channel.ForeignMedia
    -> IO Channel.PossessMediaOutcome
testPossessMedia pexpectations chatId media = do
    expectAction pexpectations $ PossessMedia chatId media

testUpdateMessage ::
       IORef [ActionExpectation]
    -> Channel.ChatId
    -> Channel.MessageId
    -> Channel.RichText
    -> [Channel.QueryButton]
    -> IO (Either Text.Text ())
testUpdateMessage pexpectations chatId messageId text buttons = do
    expectAction pexpectations $ UpdateMessage chatId messageId text buttons

testAnswerQuery ::
       IORef [ActionExpectation]
    -> Channel.QueryId
    -> Text.Text
    -> IO (Either Text.Text ())
testAnswerQuery pexpectations queryId text = do
    expectAction pexpectations $ AnswerQuery queryId text

expectAction :: IORef [ActionExpectation] -> Action a -> IO a
expectAction pexpectations action = do
    expectations <- readIORef pexpectations
    case expectations of
        top@(ActionExpectation expaction _ withStack):rest -> do
            withStack shouldBe (AnAction action) (AnAction expaction)
            writeIORef pexpectations $! rest
            return $ matchAction action top
        [] -> do
            expectationFailure $ "Unexpected action: " ++ show action
            undefined

matchAction :: Action a -> ActionExpectation -> a
matchAction SendMessage {} (ActionExpectation SendMessage {} r _) = r
matchAction SendMedia {} (ActionExpectation SendMedia {} r _) = r
matchAction PossessMedia {} (ActionExpectation PossessMedia {} r _) = r
matchAction UpdateMessage {} (ActionExpectation UpdateMessage {} r _) = r
matchAction AnswerQuery {} (ActionExpectation AnswerQuery {} r _) = r
matchAction _ _ = undefined
