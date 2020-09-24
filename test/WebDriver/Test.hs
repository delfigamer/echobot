{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}


module WebDriver.Test
    ( Action(..)
    , ActionExpectation
    , (|>>)
    , withTestDriver
    , perform
    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.IORef
import Data.List
import Data.Maybe
import Network.HTTP.Req
import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Encoding as TextEncoding
import qualified System.IO as IO
import qualified Channel
import qualified Logger
import qualified Responder
import qualified Responder.Repeat
import qualified WebDriver


data Action a where
    Request :: WebDriver.Address -> [WebDriver.Param] -> Action Value
    Download :: WebDriver.Address -> Action BSL.ByteString
deriving instance Show (Action a)


data ActionExpectation where
    ActionExpectation
        :: Show a
        => Action a
        -> a
        -> (forall t u . (HasCallStack => t -> u) -> (t -> u))
        -> ActionExpectation


instance Show ActionExpectation where
    showsPrec d (ActionExpectation action result _) = showParen (d > 0)
        $ showsPrec 1 action
        . showString " |>> "
        . showsPrec 1 result


infixr 0 |>>
(|>>) :: (Show a, HasCallStack) => Action a -> a -> ActionExpectation
action |>> result = ActionExpectation action result id


data AnAction = forall a . AnAction (Action a)


instance Show AnAction where
    showsPrec d (AnAction (Request address params)) = showParen (d > 10)
        $ showString "Request "
        . showsPrec 11 address
        . showString " "
        . showsPrec 11 (normalizeParamSet params)
    showsPrec d (AnAction (Download address)) = showParen (d > 10)
        $ showString "Download "
        . showsPrec 11 address


instance Eq AnAction where
    AnAction (Request address1 params1) == AnAction (Request address2 params2) = True
        && address1 == address2
        && normalizeParamSet params1 == normalizeParamSet params2
    AnAction (Download address1) == AnAction (Download address2) = address1 == address2
    _ == _ = False


normalizeParamSet :: [WebDriver.Param] -> [WebDriver.Param]
normalizeParamSet params = sortOn WebDriver.paramName $ map normalize $ params
    where
    normalize (WebDriver.ParamTextLazy name text) = WebDriver.ParamText name $ TextLazy.toStrict text
    normalize (WebDriver.ParamNum name num) = WebDriver.ParamText name $ Text.pack $ show num
    normalize param = param


withTestDriver
    :: (IORef [ActionExpectation] -> WebDriver.Handle -> IO r)
    -> IO r
withTestDriver body = do
    pexpectations <- newIORef $ []
    body pexpectations $ WebDriver.Handle
        { WebDriver.request = testRequest pexpectations
        , WebDriver.download = testDownload pexpectations }


testRequest :: FromJSON b => IORef [ActionExpectation] -> WebDriver.Address -> [WebDriver.Param] -> IO b
testRequest pexpectations address params = do
    value <- expectAction pexpectations $ Request address params
    case fromJSON value of
        Error err -> fail err
        Success x -> return x


testDownload :: IORef [ActionExpectation] -> WebDriver.Address -> IO BSL.ByteString
testDownload pexpectations address = do
    expectAction pexpectations $ Download address


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
matchAction (Request {}) (ActionExpectation Request {} r _) = r
matchAction (Download {}) (ActionExpectation Download {} r _) = r
matchAction _ _ = undefined


perform
    :: (Show r, Eq r)
    => IORef [ActionExpectation]
    -> IO r
    -> [ActionExpectation]
    -> (r -> IO q)
    -> IO q
perform pbuf act reqs rettest = do
    writeIORef pbuf $! reqs
    ret <- act
    bufrest <- readIORef pbuf
    bufrest `shouldSatisfy` null
    rettest ret
