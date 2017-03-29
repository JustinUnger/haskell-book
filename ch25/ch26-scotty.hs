{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty

import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Web.Scotty.Internal.Types (ActionT(..))

main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        ActionT . lift . lift . lift $ putStrLn "hello"
        html $ mconcat ["<hi>Scotty, ", beam, " me up!</h1>"]

