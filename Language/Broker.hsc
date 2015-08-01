--------------------------------------------------------------------------------
-- | 
-- Module      : Language.Broker
-- Note        : Bindings to Bro's messaging library.
-- 
-- Provides type-safe Haskell bindings to Broker, Bro's messaging library.
-- 
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   July 28, 2015
--
-- Copyright (c) 2015 David Banas; all rights reserved.
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Language.Broker (
    brokerInit
  , endpoint
  , peerRemotely
  , peerStatus
  , DottedQuad(..)
  , Endpoint
  , Peering
  , Queue
) where

import Foreign
    hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS

#include <broker.h>

newtype BrokerFlag = BrokerFlag {unBrokerFlag :: CInt}
    deriving (Eq, Show)
#{enum BrokerFlag, BrokerFlag
  , kBrokerAutoPublish      = BROKER_AUTO_PUBLISH
  , kBrokerAutoAdvertise    = BROKER_AUTO_ADVERTISE
  }
combineFlags :: [BrokerFlag] -> BrokerFlag
combineFlags = BrokerFlag . foldr ((.|.) . unBrokerFlag) 0

data Endpoint
data Peering
data Queue

data DottedQuad = DottedQuad {
    first  :: Int
  , second :: Int
  , third  :: Int
  , fourth :: Int
} deriving (Eq, Ord)
instance Show DottedQuad where
    show dq = show (first dq) ++ "." ++ show (second dq) ++ "." ++ show (third dq) ++ "." ++ show (fourth dq)

foreign import ccall "broker.h broker_init"
     c_broker_init :: IO CInt
brokerInit :: Maybe ()
brokerInit = unsafePerformIO $ do
    res <- c_broker_init
    return $ if res /= 0
               then Nothing
               else Just ()

foreign import ccall "broker.h broker_endpoint_create"
    c_broker_endpoint_create :: CString -> IO (Ptr Endpoint)
endpoint :: String -> Maybe (ForeignPtr Endpoint)
endpoint name = unsafePerformIO $
    withCString name $ \cs -> do
        res <- c_broker_endpoint_create cs
        if res == nullPtr
            then return Nothing
            else do
                fp <- newForeignPtr finalizerFree res
                return $ Just fp

foreign import ccall "broker.h broker_endpoint_peer_remotely"
    c_broker_endpoint_peer_remotely :: Ptr Endpoint -> CString -> CUInt -> CDouble -> IO (Ptr Peering)
peerRemotely :: ForeignPtr Endpoint -> String -> Int -> Maybe (ForeignPtr Peering)
peerRemotely ep addr port = unsafePerformIO $
    withForeignPtr ep $ \p -> do
        withCString addr $ \cs -> do
            res <- c_broker_endpoint_peer_remotely p cs (fromIntegral port) 5
            if res == nullPtr
                then return Nothing
                else do
                    fp <- newForeignPtr finalizerFree res
                    return $ Just fp

foreign import ccall "broker.h broker_endpoint_outgoing_connection_status"
    c_broker_endpoint_outgoing_connection_status :: Ptr Endpoint -> IO (Ptr Queue)
peerStatus :: ForeignPtr Endpoint -> Maybe (Ptr Queue)
peerStatus ep = unsafePerformIO $
    withForeignPtr ep $ \p -> do
        res <- c_broker_endpoint_outgoing_connection_status p
        return $ if res == nullPtr
            then Nothing
            else Just res
        
