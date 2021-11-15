{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Test.Ouroboros.Network.Diffusion.Node
  ( -- * run a node
    Node.BlockGeneratorArgs (..)
  , Node.LimitsAndTimeouts (..)
  , Interfaces (..)
  , Arguments (..)
  , run

    -- * node types
  , NtNAddr
  , NtNFD
  , NtCAddr
  , NtCFD

    -- * extra types used by the node
  , AcceptedConnectionsLimit (..)
  , DiffusionMode (..)
  , LedgerPeersConsensusInterface (..)
  , PeerAdvertise (..)
  , PeerSelectionTargets (..)
  , RelayAccessPoint (..)
  , UseLedgerAfter (..)
  ) where

import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadAsync
                   ( MonadAsync(wait, Async, withAsync) )
import           Control.Monad.Class.MonadFork ( MonadFork )
import           Control.Monad.IOSim
import           Control.Monad.Class.MonadST ( MonadST )
import           Control.Monad.Class.MonadSTM.Strict
                   ( MonadSTM(atomically, STM), newTVar, MonadLabelledSTM )
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadTime ( MonadTime, DiffTime )
import           Control.Monad.Class.MonadTimer ( MonadTimer )
import           Control.Monad.Class.MonadThrow
                   ( MonadMask, MonadThrow, SomeException, MonadCatch,
                     MonadEvaluate )
import           Control.Tracer (nullTracer)

import qualified Data.ByteString.Lazy as BL
import qualified Data.IntPSQ as IntPSQ
import           Data.IP (IPv4, toIPv4, IP (IPv4))
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Void (Void)
import           System.Random (StdGen, split, mkStdGen)

import qualified Codec.CBOR.Term as CBOR

import           Network.DNS (Domain)

import           Ouroboros.Network.BlockFetch.Decision (FetchMode (..))
import           Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import qualified Ouroboros.Network.Diffusion as Diff
import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P
import           Ouroboros.Network.Driver.Limits
                   (ProtocolSizeLimits(..), ProtocolTimeLimits (..))
import           Ouroboros.Network.Mux (MiniProtocolLimits(..))
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.ChainSync.Codec
                   (byteLimitsChainSync, timeLimitsChainSync,
                      ChainSyncTimeout (..))
import           Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import           Ouroboros.Network.Protocol.Handshake.Codec
                   ( noTimeLimitsHandshake,
                     VersionDataCodec(..),
                     timeLimitsHandshake )
import           Ouroboros.Network.Protocol.Handshake.Unversioned
                   ( unversionedHandshakeCodec, unversionedProtocolDataCodec )
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))
import           Ouroboros.Network.Protocol.KeepAlive.Codec
                   (byteLimitsKeepAlive, timeLimitsKeepAlive)
import           Ouroboros.Network.Protocol.Limits (shortWait, smallByteLimit)
import           Ouroboros.Network.RethrowPolicy
                   ( ioErrorRethrowPolicy,
                     mkRethrowPolicy,
                     muxErrorRethrowPolicy,
                     ErrorCommand(ShutdownNode) )
import           Ouroboros.Network.PeerSelection.Governor
                   (PeerSelectionTargets (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
                   (LedgerPeersConsensusInterface (..), UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                   (DomainAccessPoint (..), RelayAccessPoint (..), PortNumber)
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.Server.RateLimiting
                   (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Snocket (FileDescriptor (..), Snocket,
                   TestAddress (..))

import           Ouroboros.Network.Testing.ConcreteBlock (Block)
import qualified Ouroboros.Network.Testing.Data.Script as Script

import           Simulation.Network.Snocket
                   ( AddressType(IPv4Address), FD, withSnocket,
                   BearerInfo )

import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel (NtNAddr,
                   NtNVersion, NtNVersionData (..), NtCAddr, NtCVersion,
                   NtCVersionData, randomBlockGenerationArgs)
import           Test.Ouroboros.Network.PeerSelection.RootPeersDNS (DNSTimeout,
                   DNSLookupDelay, mockDNSActions)
import qualified Test.Ouroboros.Network.Diffusion.Node.NodeKernel    as Node
import qualified Test.Ouroboros.Network.Diffusion.Node.MiniProtocols as Node

import Test.QuickCheck
    ( Arbitrary(arbitrary),
      Gen,
      choose,
      chooseInt,
      chooseInteger,
      sized,
      vectorOf, Property, counterexample )

data Interfaces m = Interfaces
    { iNtnSnocket        :: Snocket m (NtNFD m) NtNAddr
    , iAcceptVersion     :: NtNVersionData -> NtNVersionData -> Accept NtNVersionData
    , iNtnDomainResolver :: [DomainAccessPoint] -> m (Map DomainAccessPoint (Set NtNAddr))
    , iNtcSnocket        :: Snocket m (NtCFD m) NtCAddr
    , iRng               :: StdGen
    , iDomainMap         :: Map Domain [IPv4]
      -- TODO use 'IP' instead of 'IPv4'
    , iLedgerPeersConsensusInterface
                         :: LedgerPeersConsensusInterface m
    }

type NtNFD m = FD m NtNAddr
type NtCFD m = FD m NtCAddr

data Arguments m = Arguments
    { aIPv4Address          :: NtNAddr
    , aIPv6Address          :: NtNAddr
    , aAcceptedLimits       :: AcceptedConnectionsLimit
    , aDiffusionMode        :: DiffusionMode
    , aKeepAliveInterval    :: DiffTime
    , aPingPongInterval     :: DiffTime

    , aPeerSelectionTargets :: PeerSelectionTargets
    , aReadLocalRootPeers   :: STM m [(Int, Map RelayAccessPoint PeerAdvertise)]
    , aReadPublicRootPeers  :: STM m [RelayAccessPoint]
    , aReadUseLedgerAfter   :: STM m UseLedgerAfter
    , aProtocolIdleTimeout  :: DiffTime
    , aTimeWaitTimeout      :: DiffTime
    , aDNSTimeoutScript     :: Script.Script DNSTimeout
    , aDNSLookupDelayScript :: Script.Script DNSLookupDelay
    }

-- | Multinode Diffusion Simulator Script
--
-- Does not contain a full 'Interface' type because we wouldn't be able to write
-- a Arbitrary instance for it due to the creation of Snockets needs to be done
-- in 'IOSim'.
--
newtype DiffMultiNodeScript = DiffMultiNodeScript
  { dMNSToRun :: [( Node.BlockGeneratorArgs Block StdGen
                  , Node.LimitsAndTimeouts Block
                    -- Interface types without Snockets
                  , ( NtNVersionData -> NtNVersionData -> Accept NtNVersionData
                    , [DomainAccessPoint] -> Map DomainAccessPoint
                                                 (Set NtNAddr)
                    , StdGen
                    , Map Domain [IPv4]
                    )
                  , ( NtNAddr
                    , [(Int, Map RelayAccessPoint PeerAdvertise)]
                    , PeerSelectionTargets
                    , Script.Script DNSTimeout
                    , Script.Script DNSLookupDelay
                    )
                  )
                 ]
  }

instance Arbitrary DiffMultiNodeScript where
  arbitrary = sized $ \size -> do
    let size' = size + 1 -- Always at least one node
    raps <- vectorOf size' arbitrary
    dMap <- genDomainMap raps
    toRun <- mapM (addressToRun raps dMap)
                 [ ntnToPeerAddr ip p | RelayAccessAddress ip p <- raps ]
    return (DiffMultiNodeScript toRun)
    where
      ntnToPeerAddr :: IP -> PortNumber -> NtNAddr
      ntnToPeerAddr = \a b -> TestAddress (Node.IPAddr a b)

      -- | Generate DNS table
      genDomainMap :: [RelayAccessPoint] -> Gen (Map Domain [IPv4])
      genDomainMap raps = do
        let domains = [ d | RelayAccessDomain d _ <- raps ]
        m <- mapM (\d -> do
          size <- chooseInt (1, 5)
          ips <- vectorOf size (toIPv4 <$> replicateM 4 (choose (0,255)))
          return (d, ips)) domains

        return (Map.fromList m)

      -- | Generate Local Root Peers
      --
      -- Only 1 group is generated
      genLocalRootPeers :: [RelayAccessPoint]
                        -> Gen [(Int, Map RelayAccessPoint PeerAdvertise)]
      genLocalRootPeers l = do
        let size = length l
        target <- chooseInt (1, size)
        peerAdvertise <- vectorOf size arbitrary
        let mapRelays = Map.fromList $ zip l peerAdvertise

        return [(target, mapRelays)]

      -- | Given a NtNAddr generate the necessary things to run in Simulation
      addressToRun :: [RelayAccessPoint]
                   -> Map Domain [IPv4]
                   -> NtNAddr
                   -> Gen ( Node.BlockGeneratorArgs Block StdGen
                          , Node.LimitsAndTimeouts Block
                            -- Interface types without Snockets
                          , ( NtNVersionData -> NtNVersionData -> Accept NtNVersionData
                            , [DomainAccessPoint] -> Map DomainAccessPoint
                                                         (Set NtNAddr)
                            , StdGen
                            , Map Domain [IPv4]
                            )
                          , ( NtNAddr
                            , [(Int, Map RelayAccessPoint PeerAdvertise)]
                            , PeerSelectionTargets
                            , Script.Script DNSTimeout
                            , Script.Script DNSLookupDelay
                            )
                          )
      addressToRun raps dMap rap = do
        blockGeneratorArgs <- randomBlockGenerationArgs
                              <$> (fromInteger <$> chooseInteger (0, 100))
                              <*> (mkStdGen <$> arbitrary)
                              <*> chooseInt (0, 100)

        let defaultMiniProtocolsLimit =
              MiniProtocolLimits { maximumIngressQueue = 64000 }

        chainSyncTimeLimits <- timeLimitsChainSync <$> stdChainSyncTimeout

        let limitsAndTimeouts :: Node.LimitsAndTimeouts Block
            limitsAndTimeouts
              = Node.LimitsAndTimeouts
                  defaultMiniProtocolsLimit
                  (byteLimitsChainSync (const 0))
                  chainSyncTimeLimits
                  defaultMiniProtocolsLimit
                  (byteLimitsKeepAlive (const 0))
                  timeLimitsKeepAlive
                  defaultMiniProtocolsLimit
                  (ProtocolSizeLimits (const smallByteLimit) (const 0))
                  (ProtocolTimeLimits (const (Just 60)))
                  defaultMiniProtocolsLimit
                  (ProtocolSizeLimits (const (4 * 1440))
                                      (fromIntegral . BL.length))
                  (ProtocolTimeLimits (const shortWait))

        interfaces <- do
          let acceptVersion = \_ v -> Accept v
              domainRes = domainResolver raps dMap

          stdGen <- mkStdGen <$> arbitrary

          return (acceptVersion, domainRes, stdGen, dMap)

        arguments <- do
          lrp <- genLocalRootPeers raps

          peerSelectionTargets <- arbitrary
          dnsTimeout <- arbitrary
          dnsLookupDelay <- arbitrary

          return
            ( rap
            , lrp
            , peerSelectionTargets
            , dnsTimeout
            , dnsLookupDelay)

        return (blockGeneratorArgs, limitsAndTimeouts, interfaces, arguments)

      domainResolver :: [RelayAccessPoint]
                     -> Map Domain [IPv4]
                     -> [DomainAccessPoint]
                     -> Map DomainAccessPoint (Set NtNAddr)
      domainResolver raps dMap daps = do
        let domains    = [ (d, p) | RelayAccessDomain d p <- raps ]
            domainsAP  = uncurry DomainAccessPoint <$> domains
            mapDomains = [ ( DomainAccessPoint d p
                           , Set.fromList
                           $ uncurry ntnToPeerAddr
                           <$> zip (IPv4 <$> dMap Map.! d) (repeat p)
                           )
                         | DomainAccessPoint d p <- domainsAP \\ daps
                         , Map.member d dMap
                         ]
        Map.fromList mapDomains

      -- Taken from ouroboros-consensus/src/Ouroboros/Consensus/Node.hs
      stdChainSyncTimeout :: Gen ChainSyncTimeout
      stdChainSyncTimeout = do
          -- These values approximately correspond to false positive
          -- thresholds for streaks of empty slots with 99% probability,
          -- 99.9% probability up to 99.999% probability.
          -- t = T_s [log (1-Y) / log (1-f)]
          -- Y = [0.99, 0.999...]
          -- T_s = slot length of 1s.
          -- f = 0.05
          -- The timeout is randomly picked per bearer to avoid all bearers
          -- going down at the same time in case of a long streak of empty
          -- slots. TODO: workaround until peer selection governor.
          mustReplyTimeout <- Just <$> randomElem [90, 135, 180, 224, 269]
          return ChainSyncTimeout
            { canAwaitTimeout  = shortWait
            , intersectTimeout = shortWait
            , mustReplyTimeout
            }
        where
          randomElem xs = do
            ix <- choose (0, length xs - 1)
            return $ xs !! ix

-- The 'mockDNSActions' is not using \/ specifying 'resolverException', thus we
-- set it to 'SomeException'.
--
type ResolverException = SomeException

run :: forall resolver m.
       ( MonadAsync       m
       , MonadEvaluate    m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadST          m
       , MonadTime        m
       , MonadTimer       m
       , MonadThrow       m
       , MonadThrow       (STM m)

       , resolver ~ ()
       , forall a. Semigroup a => Semigroup (m a)
       , Eq (Async m Void)
       )
    => Node.BlockGeneratorArgs Block StdGen
    -> Node.LimitsAndTimeouts Block
    -> Interfaces m
    -> Arguments m
    -> m Void
run blockGeneratorArgs limits ni na =
    Node.withNodeKernelThread blockGeneratorArgs
      $ \ nodeKernel nodeKernelThread -> do
        dnsTimeoutScriptVar <- LazySTM.newTVarIO (aDNSTimeoutScript na)
        dnsLookupDelayScriptVar <- LazySTM.newTVarIO (aDNSLookupDelayScript na)
        peerMetrics  <- atomically $ PeerMetrics
          <$> newTVar IntPSQ.empty
          <*> newTVar IntPSQ.empty
        let -- diffusion interfaces
            interfaces :: Diff.P2P.Interfaces (NtNFD m) NtNAddr NtNVersion NtNVersionData
                                              (NtCFD m) NtCAddr NtCVersion NtCVersionData
                                              resolver ResolverException
                                              m
            interfaces = Diff.P2P.Interfaces
              { Diff.P2P.diNtnSnocket            = iNtnSnocket ni
              , Diff.P2P.diNtnHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer      = nullTracer
                    , haHandshakeCodec       = unversionedHandshakeCodec
                    , haVersionDataCodec     = ntnUnversionedDataCodec
                    , haAcceptVersion        = iAcceptVersion ni
                    , haTimeLimits           = timeLimitsHandshake
                    }
              , Diff.P2P.diNtnAddressType    = const (Just IPv4Address)
              , Diff.P2P.diNtnDataFlow       = \_ NtNVersionData { ntnDiffusionMode } ->
                  case ntnDiffusionMode of
                    InitiatorOnlyDiffusionMode         -> Unidirectional
                    InitiatorAndResponderDiffusionMode -> Duplex
              , Diff.P2P.diNtnToPeerAddr         = \a b -> TestAddress (Node.IPAddr a b)
              , Diff.P2P.diNtnDomainResolver     = iNtnDomainResolver ni
              , Diff.P2P.diNtcSnocket            = iNtcSnocket ni
              , Diff.P2P.diNtcHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer      = nullTracer
                    , haHandshakeCodec       = unversionedHandshakeCodec
                    , haVersionDataCodec     = unversionedProtocolDataCodec
                    , haAcceptVersion        = \_ v -> Accept v
                    , haTimeLimits           = noTimeLimitsHandshake
                    }
              , Diff.P2P.diNtcGetFileDescriptor  = \_ -> pure (FileDescriptor (-1))
              , Diff.P2P.diRng                   = diffStgGen
              , Diff.P2P.diInstallSigUSR1Handler = \_ -> pure ()
              , Diff.P2P.diDnsActions            = mockDNSActions
                                                     (iDomainMap ni)
                                                     dnsTimeoutScriptVar
                                                     dnsLookupDelayScriptVar
              }

            tracersExtra :: Diff.P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                                                  NtCAddr NtCVersion NtCVersionData
                                                  ResolverException m
            tracersExtra = Diff.P2P.nullTracers

            appsExtra :: Diff.P2P.ApplicationsExtra NtNAddr m
            appsExtra = Diff.P2P.ApplicationsExtra
              { Diff.P2P.daMiniProtocolParameters = NtN.defaultMiniProtocolParameters
                -- TODO: simulation errors should be critical
              , Diff.P2P.daRethrowPolicy          =
                     muxErrorRethrowPolicy
                  <> ioErrorRethrowPolicy

                -- we are not using local connections, so we can make all the
                -- errors fatal.
              , Diff.P2P.daLocalRethrowPolicy     =
                     mkRethrowPolicy
                       (\ _ (_ :: SomeException) -> ShutdownNode)
              , Diff.P2P.daPeerMetrics            = peerMetrics
                -- fetch mode is not used (no block-fetch mini-protocol)
              , Diff.P2P.daBlockFetchMode         = pure FetchModeDeadline
              }

        apps <- Node.applications nodeKernel Node.cborCodecs limits appArgs

        withAsync
           (Diff.P2P.runM interfaces
                          Diff.nullTracers tracersExtra
                          args argsExtra apps appsExtra)
           $ \ diffusionThread ->
               wait diffusionThread
            <> wait nodeKernelThread
  where
    -- various pseudo random generators
    (diffStgGen, keepAliveStdGen) = split (iRng ni)

    ntnUnversionedDataCodec :: VersionDataCodec CBOR.Term NtNVersion NtNVersionData
    ntnUnversionedDataCodec = VersionDataCodec { encodeData, decodeData }
      where
        encodeData _ NtNVersionData { ntnDiffusionMode } =
          case ntnDiffusionMode of
            InitiatorOnlyDiffusionMode         -> CBOR.TBool False
            InitiatorAndResponderDiffusionMode -> CBOR.TBool True
        decodeData _ bytes = case bytes of
          CBOR.TBool False -> Right (NtNVersionData InitiatorOnlyDiffusionMode)
          CBOR.TBool True  -> Right (NtNVersionData InitiatorAndResponderDiffusionMode)
          _                -> Left (Text.pack "unversionedDataCodec: unexpected term")

    args :: Diff.Arguments (NtNFD m) NtNAddr (NtCFD m) NtCAddr
    args = Diff.Arguments
      { Diff.daIPv4Address   = Just . Right . aIPv4Address $ na
      , Diff.daIPv6Address   = Just . Right . aIPv6Address $ na
      , Diff.daLocalAddress  = Nothing
      , Diff.daAcceptedConnectionsLimit
                             = aAcceptedLimits na
      , Diff.daMode          = aDiffusionMode na
      }

    argsExtra :: Diff.P2P.ArgumentsExtra m
    argsExtra = Diff.P2P.ArgumentsExtra
      { Diff.P2P.daPeerSelectionTargets = aPeerSelectionTargets na
      , Diff.P2P.daReadLocalRootPeers   = aReadLocalRootPeers na
      , Diff.P2P.daReadPublicRootPeers  = aReadPublicRootPeers na
      , Diff.P2P.daReadUseLedgerAfter   = aReadUseLedgerAfter na
      , Diff.P2P.daProtocolIdleTimeout  = aProtocolIdleTimeout na
      , Diff.P2P.daTimeWaitTimeout      = aTimeWaitTimeout na
      }

    appArgs :: Node.AppArgs m
    appArgs = Node.AppArgs
      { Node.aaLedgerPeersConsensusInterface
                                        = iLedgerPeersConsensusInterface ni
      , Node.aaKeepAliveStdGen          = keepAliveStdGen
      , Node.aaDiffusionMode            = aDiffusionMode na
      , Node.aaKeepAliveInterval        = aKeepAliveInterval na
      , Node.aaPingPongInterval         = aPingPongInterval na
      }

-- | Run an arbitrary topology
multinodeDiffusionSim :: ( MonadAsync m
                         , MonadFork m
                         , MonadST m
                         , MonadEvaluate m
                         , MonadLabelledSTM m
                         , MonadCatch       m
                         , MonadMask        m
                         , MonadTime        m
                         , MonadTimer       m
                         , MonadThrow  (STM m)
                         , Eq (Async m Void)
                         , forall a. Semigroup a => Semigroup (m a)
                         )
                      => Script.Script BearerInfo
                      -> DiffMultiNodeScript
                      -> m Void
multinodeDiffusionSim _ (DiffMultiNodeScript []) = error "Impossible happened!"
multinodeDiffusionSim
  script
  (DiffMultiNodeScript ((bga, lat, int, ar) : xs))
  = withSnocket nullTracer script
      $ \ntnSnocket -> withSnocket nullTracer script
      $ \ntcSnocket ->
        let (ntnAcceptF, domainRes, stdGen, dMap) = int
            (rap, lrp, pst, dnsT, dnsL) = ar

            acceptedConnectionsLimit =
              AcceptedConnectionsLimit maxBound maxBound 0
            diffusionMode = InitiatorAndResponderDiffusionMode
            readPRP = return []
            readULA = return (UseLedgerAfter 0)

            interfaces =
              Interfaces ntnSnocket
                         ntnAcceptF
                         (return <$> domainRes)
                         ntcSnocket
                         stdGen
                         dMap
                         (LedgerPeersConsensusInterface $ \_ -> return Nothing)
            arguments =
              Arguments rap
                        rap
                        acceptedConnectionsLimit
                        diffusionMode
                        0
                        0
                        pst
                        (return lrp)
                        readPRP
                        readULA
                        5
                        30
                        dnsT
                        dnsL

         in run bga lat interfaces arguments

test :: Script.Script BearerInfo
     -> DiffMultiNodeScript
     -> Property
test script dmnScript =
  let trace = runSimTrace (multinodeDiffusionSim script dmnScript)
   in counterexample (ppTrace trace) False
