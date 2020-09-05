module Responder
    ( Handle(..)
    ) where


data Handle
    = Handle
        { work :: IO () }
