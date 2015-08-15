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
  , bstringToString
  , createMsgQueue
  , dataToString
  , endpoint
  , fetchMsgs
  , getMsg
  , getMsgItem
  , getStatus
  , makeString
  , msgQueueSize
  , peerLocally
  , peerRemotely
  , peerStatus
  , queueSize
  , sendMsg
  , DottedQuad(..)
  , Endpoint
  , Peering
  , Queue
  , Dequeue
  , kEndpointAutoPublish
  , kEndpointAutoAdvertise
) where

import Foreign
    hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS

#include <broker.h>

newtype EndpointFlag = EndpointFlag {unEndpointFlag :: CInt}
    deriving (Eq, Show)
#{enum EndpointFlag, EndpointFlag
  , kEndpointAutoPublish      = BROKER_AUTO_PUBLISH
  , kEndpointAutoAdvertise    = BROKER_AUTO_ADVERTISE
  }
combineFlags :: [EndpointFlag] -> EndpointFlag
combineFlags = EndpointFlag . foldr ((.|.) . unEndpointFlag) 0

data Endpoint            -- broker_endpoint
data Peering             -- broker_peering
data Queue               -- broker_outgoing_connection_status_queue
data Dequeue             -- broker_deque_of_outgoing_connection_status
data BString             -- broker_string
data BVector             -- broker_vector
data BData               -- broker_data
data Msg                 -- broker_message
data MsgQueue            -- broker_message_queue
data MsgList             -- broker_deque_of_message

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
brokerInit :: Either String ()
brokerInit = unsafePerformIO $ do
    res <- c_broker_init
    return $ if res /= 0
               then (Left "brokerInit: Failed!")
               else (Right ())

foreign import ccall "broker.h broker_endpoint_create_with_flags"
    c_broker_endpoint_create_with_flags :: CString -> CInt -> IO (Ptr Endpoint)
endpoint :: String -> [EndpointFlag] -> Either String (ForeignPtr Endpoint)
endpoint name flags = unsafePerformIO $
    withCString name $ \cs -> do
        res <- c_broker_endpoint_create_with_flags cs $ unEndpointFlag $ combineFlags flags
        if res == nullPtr
            then return $ Left "endpoint: Failed!"
            else do
                fp <- newForeignPtr finalizerFree res
                return $ Right fp

foreign import ccall "broker.h broker_endpoint_peer_locally"
    c_broker_endpoint_peer_locally :: Ptr Endpoint -> Ptr Endpoint -> IO (Ptr Peering)
peerLocally :: ForeignPtr Endpoint -> ForeignPtr Endpoint -> Either String (ForeignPtr Peering)
peerLocally ep1 ep2 = unsafePerformIO $
    withForeignPtr ep1 $ \p1 -> do
        withForeignPtr ep2 $ \p2 -> do
            res <- c_broker_endpoint_peer_locally p1 p2
            if res == nullPtr
                then return $ Left "peerLocally: Failed!"
                else do
                    fp <- newForeignPtr finalizerFree res
                    return $ Right fp

foreign import ccall "broker.h broker_endpoint_peer_remotely"
    c_broker_endpoint_peer_remotely :: Ptr Endpoint -> CString -> CUInt -> CDouble -> IO (Ptr Peering)
peerRemotely :: ForeignPtr Endpoint -> String -> Int -> Either String (ForeignPtr Peering)
peerRemotely ep addr port = unsafePerformIO $
    withForeignPtr ep $ \p -> do
        withCString addr $ \cs -> do
            res <- c_broker_endpoint_peer_remotely p cs (fromIntegral port) 5
            if res == nullPtr
                then return $ Left "peerRemotely: Failed!"
                else do
                    fp <- newForeignPtr finalizerFree res
                    return $ Right fp

foreign import ccall "broker.h broker_endpoint_outgoing_connection_status"
    c_broker_endpoint_outgoing_connection_status :: Ptr Endpoint -> IO (Ptr Queue)
peerStatus :: ForeignPtr Endpoint -> Either String (Ptr Queue)
peerStatus ep = unsafePerformIO $
    withForeignPtr ep $ \p -> do
        res <- c_broker_endpoint_outgoing_connection_status p
        return $ if res == nullPtr
            then (Left "peerStatus: Failed!")
            else (Right res)
        
--foreign import ccall "broker.h broker_outgoing_connection_status_queue_want_pop"
foreign import ccall "broker.h broker_outgoing_connection_status_queue_need_pop"
    c_broker_outgoing_connection_status_queue_want_pop :: Ptr Queue -> IO (Ptr Dequeue)
getStatus :: Ptr Queue -> Either String (ForeignPtr Dequeue)
getStatus pq = unsafePerformIO $ do
    res <- c_broker_outgoing_connection_status_queue_want_pop pq
    if res == nullPtr
        then return $ Left "getStatus: Failed!"
        else do
            fp <- newForeignPtr finalizerFree res
            return $ Right fp

foreign import ccall "broker.h broker_deque_of_outgoing_connection_status_size"
    c_broker_deque_of_outgoing_connection_status_size :: Ptr Dequeue -> IO CULong
queueSize :: ForeignPtr Dequeue -> Int
queueSize x = unsafePerformIO $
    withForeignPtr x $ \p -> do
        res <- c_broker_deque_of_outgoing_connection_status_size p
        return $ fromIntegral res

foreign import ccall "broker.h broker_string_create"
    c_broker_string_create :: CString -> IO (Ptr BString)
makeString :: String -> Either String (ForeignPtr BString)
makeString s = unsafePerformIO $ do
    withCString s $ \cs -> do
        res <- c_broker_string_create cs
        if res == nullPtr
            then return $ Left "makeString: Failed!"
            else do
                fp <- newForeignPtr finalizerFree res
                return $ Right fp

foreign import ccall "broker.h broker_data_as_vector"
    c_broker_data_as_vector :: Ptr BData -> IO (Ptr BVector)
dataAsVector :: ForeignPtr BData -> Either String (Ptr BVector)
dataAsVector fp_bd = unsafePerformIO $ do
    withForeignPtr fp_bd $ \p_bd -> do
        res <- c_broker_data_as_vector p_bd
        if res == nullPtr
            then return $ Left "dataAsVector: Failed!"
            else return $ Right res

foreign import ccall "broker.h broker_data_from_string"
    c_broker_data_from_string :: Ptr BString -> IO (Ptr BData)
dataFromString :: String -> Either String (ForeignPtr BData)
dataFromString s = do
    fp_bs <- makeString s
    unsafePerformIO $ do
        withForeignPtr fp_bs $ \p_bs -> do
            res <- c_broker_data_from_string p_bs
            if res == nullPtr
                then return $ Left "dataFromString: Failed!"
                else do
                    fp <- newForeignPtr finalizerFree res
                    return $ Right fp

foreign import ccall "broker.h broker_endpoint_send"
    c_broker_endpoint_send :: Ptr Endpoint -> Ptr BString -> Ptr BVector -> IO CInt
sendMsg :: ForeignPtr Endpoint -> String -> String -> Either String ()
sendMsg ep topic msg = do
    fp_bs <- makeString topic
    fp_bd <- dataFromString msg
    fp_bv <- createVector
    vectorInsert fp_bv fp_bd 0
    unsafePerformIO $ do
        withForeignPtr ep $ \p -> do
            withForeignPtr fp_bs $ \p_bs -> do
                withForeignPtr fp_bv $ \p_bv -> do
                    res <- c_broker_endpoint_send p p_bs p_bv
                    return $ if (fromIntegral res) == 0
                                then (Left "sendMsg: Failed!")
                                else (Right ())

foreign import ccall "broker.h broker_data_to_string"
    c_broker_data_to_string :: Ptr BData -> IO (Ptr BString)
dataToString :: Ptr BData -> Either String (ForeignPtr BString)
dataToString p_bd = unsafePerformIO $ do
    res <- c_broker_data_to_string p_bd
    if res == nullPtr
        then return $ Left "dataToString: Failed!"
        else do
            fp <- newForeignPtr finalizerFree res
            return $ Right fp

foreign import ccall "broker.h broker_string_data"
    c_broker_string_data :: Ptr BString -> IO CString
bstringToString :: ForeignPtr BString -> String
bstringToString fp_bs = unsafePerformIO $ do
    withForeignPtr fp_bs $ \p_bs -> do
        cs <- c_broker_string_data p_bs
        peekCString cs

foreign import ccall "broker.h broker_vector_create"
    c_broker_vector_create :: IO (Ptr BVector)
createVector :: Either String (ForeignPtr BVector)
createVector = unsafePerformIO $ do
    res <- c_broker_vector_create
    if res == nullPtr
        then return $ Left "createVector: Failed!"
        else do
            fp <- newForeignPtr finalizerFree res
            return $ Right fp

foreign import ccall "broker.h broker_vector_insert"
    c_broker_vector_insert :: Ptr BVector -> Ptr BData -> CULong -> IO CInt
vectorInsert :: ForeignPtr BVector -> ForeignPtr BData -> Int -> Either String ()
vectorInsert fp_bv fp_bd pos = unsafePerformIO $ do
    withForeignPtr fp_bv $ \p_bv -> do
        withForeignPtr fp_bd $ \p_bd -> do
            res <- c_broker_vector_insert p_bv p_bd (fromIntegral pos)
            if (fromIntegral res) == 0
              then return $ Left "vectorInsert: Failed!"
              else return $ Right ()

foreign import ccall "broker.h broker_message_queue_create"
    c_broker_message_queue_create :: Ptr BString -> Ptr Endpoint -> IO (Ptr MsgQueue)
createMsgQueue :: ForeignPtr BString -> ForeignPtr Endpoint -> Either String (ForeignPtr MsgQueue)
createMsgQueue fp_bs fp_ep = unsafePerformIO $ do
    withForeignPtr fp_bs $ \p_bs -> do
        withForeignPtr fp_ep $ \p_ep -> do
            res <- c_broker_message_queue_create p_bs p_ep
            if res == nullPtr
                then return $ Left "createMsgQueue: Failed!"
                else do
                    fp <- newForeignPtr finalizerFree res
                    return $ Right fp

--foreign import ccall "broker.h broker_message_queue_want_pop"
foreign import ccall "broker.h broker_message_queue_need_pop"
    c_broker_message_queue_want_pop :: Ptr MsgQueue -> IO (Ptr MsgList)
fetchMsgs :: ForeignPtr MsgQueue -> Either String (ForeignPtr MsgList)
fetchMsgs msgQueue = unsafePerformIO $ do
    withForeignPtr msgQueue $ \msgQueue' -> do
        res <- c_broker_message_queue_want_pop msgQueue'
        if res == nullPtr
            then return $ Left "fetchMsgs: Failed!"
            else do
                fp <- newForeignPtr finalizerFree res
                return $ Right fp

foreign import ccall "broker.h broker_deque_of_message_at"
    c_broker_deque_of_message_at :: Ptr MsgList -> CULong -> IO (Ptr Msg)
getMsg :: ForeignPtr MsgList -> Int -> Either String (Ptr Msg)
getMsg msgList ix = unsafePerformIO $ do
    withForeignPtr msgList $ \msgList' -> do
        res <- c_broker_deque_of_message_at msgList' (fromIntegral ix)
        if res == nullPtr
            then return $ Left "getMsg: Failed!"
            else return $ Right res

foreign import ccall "broker.h broker_vector_lookup"
    c_broker_vector_lookup :: Ptr Msg -> CULong -> IO (Ptr BData)
getMsgItem :: Ptr Msg -> Int -> Either String (Ptr BData)
getMsgItem msg ix = unsafePerformIO $ do
    res <- c_broker_vector_lookup msg (fromIntegral ix)
    if res == nullPtr
        then return $ Left "getMsgItem: Failed!"
        else return $ Right res

foreign import ccall "broker.h broker_deque_of_message_size"
    c_broker_deque_of_message_size :: Ptr MsgList -> IO CULong
msgQueueSize :: ForeignPtr MsgList -> Int
msgQueueSize x = unsafePerformIO $
    withForeignPtr x $ \p -> do
        res <- c_broker_deque_of_message_size p
        return $ fromIntegral res

