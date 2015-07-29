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

import Control.Monad
import Language.Broker

hostStr = "127.0.0.1:47758"

main :: IO ()
main = do
    let res = brokerInit
    case res of
      Left e  -> putStrLn e
      Right r -> do
        let ep = endpoint "ep_1"
        case ep of
          Nothing -> putStrLn "Failure!"
          Just _ -> putStrLn "Success!"

