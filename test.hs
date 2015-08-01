--------------------------------------------------------------------------------
-- | 
-- Module      : Main
-- Note        : Testing of Language.Broker.
-- 
-- A simple test of the Haskell binding to Broker, Bro's messaging library.
-- 
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   July 28, 2015
--
-- Copyright (c) 2015 David Banas; all rights reserved.
--------------------------------------------------------------------------------

module Main where

import Data.Maybe
import Foreign
import Language.Broker

hostStr = "127.0.0.1"
port    = 9999

test :: Maybe (Ptr Queue)
test = do
    brokerInit
    ep <- endpoint "ep_1"
    pr <- peerRemotely ep hostStr port
    peerStatus ep

main :: IO ()
main = putStrLn $ if isNothing test
                    then "Failure."
                    else "Success!"

