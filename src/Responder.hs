{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Responder
    (
    ) where


-- import Control.Applicative
-- import Control.Exception
-- import Control.Monad
-- import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Aeson
-- import Network.HTTP.Req
-- import qualified Data.Text as Text
-- import qualified Channel
-- import qualified Logger


-- data Handle
    -- = Handle
        -- { respond :: Channel.Message -> IO [Channel.Message] }


-- {--}


-- withEchoResponder :: Logger.Handle -> (Handle -> IO r) -> IO r
-- withEchoResponder logger body = do
    -- body $ Handle
        -- { respond = echoRespond logger }


-- echoRespond :: Logger.Handle -> Channel.Message -> IO [Channel.Message]
-- echoRespond logger message = do
    -- return [message]
