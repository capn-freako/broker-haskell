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
import Test.Hspec

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
                    -- Message fetching/unwrapping.
                    msgs <- fetchMsgs mq2
                    msg <- getMsg msgs 0
                    item <- getMsgItem msg 0
                    bs <- dataToString item
                    Right (bstringToString bs)
       in case res of
            Left s -> s
            Right bs  -> "Message received: " ++ bs
 
main :: IO ()
main = hspec $ do
  describe "Test simple message passing between endpoints." $ do
    it "Peer endpoints and pass a message between them." $ do
      test `shouldBe` "Message received: Hello, World!"

--main = putStrLn $ test

