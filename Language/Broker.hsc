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

module Language.Broker (
    brokerInit
  , DottedQuad(..)
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

newtype Broker = Broker (Ptr Broker)

data DottedQuad = DottedQuad {
    first  :: Int
  , second :: Int
  , third  :: Int
  , fourth :: Int
} deriving (Eq, Ord)
instance Show DottedQuad where
    show dq = show (first dq) ++ "." ++ show (second dq) ++ "." ++ show (third dq) ++ "." ++ show (fourth dq)

foreign import ccall "broker.h broker_init"
     c_broker_init :: IO (CInt)
brokerInit :: Either String ()
brokerInit = unsafePerformIO $ do
    res <- c_broker_init
    return $ if res /= 0
               then (Left "ERROR: Broker initialization failed!")
               else (Right ())

--foreign import ccall "Broker.h bro_conn_new_str"
--     c_bro_conn_new_str :: CString -> CInt -> IO (Ptr Broker)
--BrokerNewStr :: String -> [BrokerFlag] -> Either String (Ptr Broker)
--BrokerNewStr hostStr flags = unsafePerformIO $
--    withCString hostStr $ \cs -> do
--        res <- c_bro_conn_new_str cs $ unBrokerFlag $ combineFlags flags
--        if res == nullPtr
--          then
--            return (Left "Failed to acquire connection.")
--          else
--            return (Right res)
--
--foreign import ccall "Broker.h bro_event_registry_add"
--     c_bro_event_registry_add :: Ptr Broker -> CString -> FunPtr (Ptr Broker -> Ptr () -> Ptr CDouble -> Ptr CDouble -> Ptr CULong -> IO ()) -> Ptr () -> IO ()
--broEventRegistryAdd :: (Ptr Broker) -> String -> FunPtr (Ptr Broker -> Ptr () -> Ptr CDouble -> Ptr CDouble -> Ptr CULong -> IO ()) -> IO ()
--broEventRegistryAdd bc evStr callBack = withCString evStr $ \cs -> c_bro_event_registry_add bc cs callBack nullPtr
--
--foreign import ccall "Broker.h bro_conn_connect"
--     c_bro_conn_connect :: (Ptr Broker) -> CInt
--BrokerConnect :: (Ptr Broker) -> Bool
--BrokerConnect bc = if c_bro_conn_connect bc == 0
--                      then False
--                      else True
--
---- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer
--foreign import ccall "wrapper"
--  wrap :: (Ptr Broker -> Ptr () -> Ptr CDouble -> Ptr CDouble -> Ptr CULong -> IO ()) -> IO (FunPtr (Ptr Broker -> Ptr () -> Ptr CDouble -> Ptr CDouble -> Ptr CULong -> IO ()))
--
---- here's the function to use as a callback
--broPong :: Ptr Broker -> Ptr () -> Ptr CDouble -> Ptr CDouble -> Ptr CULong -> IO ()
--broPong _ _ _ _ _ = putStrLn "I just got ponged!"

