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

import Language.Broker

test :: String
test = let res = do brokerInit
                    -- Endpoint creation/peering.
                    ep1 <- endpoint "ep_1" [kEndpointAutoPublish, kEndpointAutoAdvertise]
                    ep2 <- endpoint "ep_2" [kEndpointAutoPublish, kEndpointAutoAdvertise]
                    ts <- makeString ""
                    mq2 <- createMsgQueue ts ep2
                    peerLocally ep2 ep1
                    -- Connection confirmation.
                    sq <- peerStatus ep2
                    getStatus sq
                    -- Message sending.
                    sendMsg ep1 "test" "Hello, World!"
                    msgs <- fetchMsgs mq2
                    return msgs
--                    Left "Debugging."
--                    msg <- getMsg msgs 0
--                    item <- getMsgItem msg 0
--                    dataToString item
       in case res of
            Left s -> s
            Right bs  -> "There are " ++ show (msgQueueSize bs) ++ " messages in the queue."

main :: IO ()
main = putStrLn $ test

