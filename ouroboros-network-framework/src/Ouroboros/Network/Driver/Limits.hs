{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE BangPatterns #-}

-- | Drivers for running 'Peer's.
--
module Ouroboros.Network.Driver.Limits (

  -- * Limits
  ProtocolSizeLimits(..),
  ProtocolTimeLimits(..),
  ProtocolLimitFailure(..),

  -- * Normal peers
  runPeerWithLimits,
  TraceSendRecv(..),

  -- * Pipelined peers
  runPipelinedPeerWithLimits,

  -- * Driver utilities
  driverWithLimits,
  ) where

import Data.Maybe (fromMaybe)

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Control.Tracer (Tracer (..), traceWith)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Driver

import Ouroboros.Network.Codec
import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Simple (TraceSendRecv(..))


data ProtocolSizeLimits ps bytes = ProtocolSizeLimits {
       sizeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Word,

       dataSize          :: bytes -> Word
     }

data ProtocolTimeLimits ps = ProtocolTimeLimits {
       timeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Maybe DiffTime,
       decodeLimit :: DiffTime
     }

data ProtocolLimitFailure = ExceededSizeLimit
                          | ExceededTimeLimit
                          | ExceededCodecTimeLimit
  deriving (Eq, Show)

instance Exception ProtocolLimitFailure


driverWithLimits :: forall ps failure bytes m.
                    (MonadThrow m, MonadTimer m, Exception failure)
                 => Tracer m (TraceSendRecv ps)
                 -> Codec ps failure m bytes
                 -> ProtocolSizeLimits ps bytes
                 -> ProtocolTimeLimits ps
                 -> Channel m bytes
                 -> Driver ps (Maybe bytes) m
driverWithLimits tracer Codec{encode, decode}
                 ProtocolSizeLimits{sizeLimitForState, dataSize}
                 ProtocolTimeLimits{timeLimitForState, decodeLimit}
                 channel@Channel{send} =
    Driver { sendMessage, recvMessage, startDState = Nothing }
  where
    sendMessage :: forall (pr :: PeerRole) (st :: ps) (st' :: ps).
                   PeerHasAgency pr st
                -> Message ps st st'
                -> m ()
    sendMessage stok msg = do
      send (encode stok msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))

    recvMessage :: forall (pr :: PeerRole) (st :: ps).
                   PeerHasAgency pr st
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage stok trailing = do
      decoder <- decode stok
      let sizeLimit = sizeLimitForState stok
          timeLimit = fromMaybe (-1) (timeLimitForState stok)
      result  <- timeout timeLimit $
                   runDecoderWithLimit sizeLimit decodeLimit dataSize
                                       channel trailing decoder
      case result of
        Just (Right x@(SomeMessage msg, _trailing')) -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return x
        Just (Left (Right failure)) -> throwM failure
        Just (Left (Left failture)) -> throwM failture
        Nothing                    -> throwM ExceededTimeLimit

runDecoderWithLimit
    :: forall m bytes failure a.
       ( MonadThrow m, MonadTimer m, Exception failure)
    => Word
    -- ^ message size limit
    -> DiffTime
    -- ^ message time limit
    -> (bytes -> Word)
    -- ^ byte size
    -> Channel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m a
    -> m (Either (Either ProtocolLimitFailure failure) (a, Maybe bytes))
runDecoderWithLimit sizeLimit timeLimit size Channel{recv} =
    go True 0
  where
    -- Our strategy here is as follows...
    --
    -- We of course want to enforce the maximum data limit, but we also want to
    -- detect and report when we exceed the limit rather than having it be
    -- misclassified as a generic decode error. For example if we simply limited
    -- the decoder input to the maximum size then the failure would be reported
    -- as an unexpected end of input, rather than that the size limit was
    -- exceeded.
    --
    -- So our strategy is to allow the last chunk of input to exceed the limit.
    -- This leaves just one special case: if the decoder finishes with that
    -- final chunk, we must check if it consumed too much of the final chunk.
    --
    go :: Bool        -- ^ First XXX
       -> Word        -- ^ size of consumed input so far
       -> Maybe bytes -- ^ any trailing data
       -> DecodeStep bytes failure m a
       -> m (Either (Either ProtocolLimitFailure failure) (a, Maybe bytes))

    go _ !sz _ (DecodeDone x trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > sizeLimit = return (Left $ Left ExceededSizeLimit)
      | otherwise   = return (Right (x, trailing))

    go _ !_ _  (DecodeFail failure) = return (Left (Right failure))

    go True sz trailing (DecodePartial k) = do
          -- Partial parcing of a new message. Start a timer.
          r <- timeout timeLimit $ partialCont sz trailing (DecodePartial k)
          --r <- Just <$> partialCont 0 trailing (DecodePartial k)
          case r of
               Nothing -> return $ Left $ Left ExceededCodecTimeLimit
               Just a  -> return a

    go False !sz trailing (DecodePartial k) = partialCont sz trailing (DecodePartial k)

    partialCont :: Word  -- ^ size of consumed input so far
       -> Maybe bytes    -- ^ any trailing data
       -> DecodeStep bytes failure m a
       -> m (Either (Either ProtocolLimitFailure failure) (a, Maybe bytes))
    partialCont !sz trailing (DecodePartial k)
      | sz > sizeLimit = return $ Left $ Left ExceededSizeLimit
      | otherwise  = case trailing of
                       Nothing -> do mbs <- recv
                                     let !sz' = sz + maybe 0 size mbs
                                     go False sz' Nothing =<< k mbs
                       Just bs -> do let sz' = sz + size bs
                                     go False sz' Nothing =<< k (Just bs)

    -- We place an upper limit of 180s on the time we wait on receiving a partial CBOR message even when
    -- there isn't an explicit limit for the current state.
    -- 180s for receiving an CBOR message of 2_097_154 bytes corresponds to a minimum speed limit of
    -- 93kbps.
    --
    -- 2_097_154 comes from the current maximum message, see `blockFetchProtocolLimits`.
    codecTimeLimit :: DiffTime
    codecTimeLimit = 2


runPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     (MonadTimer m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a
runPeerWithLimits tracer codec slimits tlimits channel peer =
    fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    driver = driverWithLimits tracer codec slimits tlimits channel


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadSTM' constraint.
--
runPipelinedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a.
     (MonadAsync m, MonadTimer m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m a
runPipelinedPeerWithLimits tracer codec slimits tlimits channel peer =
    fst <$> runPipelinedPeerWithDriver driver peer (startDState driver)
  where
    driver = driverWithLimits tracer codec slimits tlimits channel

