{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-unused-imports  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  , docTracers
  ) where

import           Data.Aeson.Types (ToJSON)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (trace)
import           Cardano.TraceDispatcher.BasicInfo.Combinators
import           Cardano.TraceDispatcher.BasicInfo.Types (BasicInfo)
import           Cardano.TraceDispatcher.ChainDB.BlockReplayProgress
import           Cardano.TraceDispatcher.ChainDB.Combinators
import           Cardano.TraceDispatcher.ChainDB.Docu
import           Cardano.TraceDispatcher.Consensus.Combinators
import           Cardano.TraceDispatcher.Consensus.Docu
import           Cardano.TraceDispatcher.Consensus.ForgingThreadStats
                     (ForgeThreadStats, docForgeStats, forgeThreadStats)
import           Cardano.TraceDispatcher.Consensus.StateInfo
import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Network.Combinators
import           Cardano.TraceDispatcher.Network.Docu
import           Cardano.TraceDispatcher.Network.Formatting ()
import           Cardano.TraceDispatcher.Peer
import           Cardano.TraceDispatcher.Resources (namesForResources,
                     severityResources, startResourceTracer)
import qualified "trace-dispatcher" Control.Tracer as NT
-- import           Cardano.TraceDispatcher.Consensus.StartLeadershipCheck


import           Cardano.Node.Configuration.Logging (EKGDirect)
import           Cardano.Node.Types (NodeInfo, docNodeInfoTraceEvent)
import           Trace.Forward.Utils.DataPoint (DataPoint)

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.OrphanInstances.Common (ToObject)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..))

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)


import           Ouroboros.Network.Block (Point (..), Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..),
                     WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..),
                     WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (SubscriptionTrace (..))
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

import           Debug.Trace

type Peer = NtN.ConnectionId Socket.SockAddr

-- | Construct tracers for all system components.
--
mkDispatchTracers
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , TraceConstraints blk
  , Show peer, Eq peer
  , Show localPeer
  , ToObject peer
  , ToObject localPeer
  , LogFormatting peer
  , LogFormatting localPeer
  )
  => BlockConfig blk
  -> TraceOptions
  -> Old.Trace IO Text
  -> NodeKernelData blk
  -> Maybe EKGDirect
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> [BasicInfo]
  -> NodeInfo
  -> IO (Tracers peer localPeer blk)
mkDispatchTracers _blockConfig (TraceDispatcher _trSel) _tr nodeKernel _ekgDirect
  trBase trForward mbTrEKG trDataPoint trConfig basicInfos nodeInfo = do
    trace ("TraceConfig " <> show trConfig) $ pure ()
    niTr <-   mkDataPointTracer
                trDataPoint
                (const ["NodeInfo"])
    traceWith niTr nodeInfo
    cdbmTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                withAddedToCurrentChainEmptyLimited
    rbTr    <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ReplayBlock"
                namesForReplayBlockStats
                severityReplayBlockStats
                allPublic
    cscTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    csshTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    cssbTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    bfdTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    bfcTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    bfsTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    fsiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                allPublic
    txiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
    txoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
    ltxsTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    mpTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Mempool"
                namesForMempool
                severityMempool
                allPublic
    fTr    <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "Forge"
                namesForForge
                severityForge
                allPublic
                (forgeTracerTransform nodeKernel)
    fSttTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                forgeThreadStats
    btTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    kacTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
    tcsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
    ttsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
    tsqTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
    tcsnTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    tcssTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    tbfTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    tbfsTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    tsnTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
    ts2nTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    ipsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
    dnssTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
    dnsrTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
    errpTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
    lerrpTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
    apTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
    muxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Mux"
                namesForMux
                severityMux
                allPublic
    muxLTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
    hsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
    lhsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
    diTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
    rsTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Resources"
                (const [])
                (const Info)
                allPublic
    biTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BasicInfo"
                namesForBasicInfo
                severityBasicInfo
                allPublic
    pTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Peers"
                namesForPeers
                severityPeers
                allPublic

    configureTracers trConfig docChainDBTraceEvent    [cdbmTr]
    configureTracers trConfig docReplayedBlock        [rbTr]
    configureTracers trConfig docChainSyncClientEvent [cscTr]
    configureTracers trConfig docChainSyncServerEvent [csshTr]
    configureTracers trConfig docChainSyncServerEvent [cssbTr]
    configureTracers trConfig docBlockFetchDecision   [bfdTr]
    configureTracers trConfig docBlockFetchClient     [bfcTr]
    configureTracers trConfig docReplayedBlock        [rbTr]
    configureTracers trConfig docBlockFetchServer     [bfsTr]
    configureTracers trConfig docForgeStateInfo       [fsiTr]
    configureTracers trConfig docTxInbound            [txiTr]
    configureTracers trConfig docTxOutbound           [txoTr]
    configureTracers trConfig docLocalTxSubmissionServer [ltxsTr]
    configureTracers trConfig docMempool              [mpTr]
    configureTracers trConfig docForge                [fTr, fSttTr]
    configureTracers trConfig docBlockchainTime       [btTr]
    configureTracers trConfig docKeepAliveClient      [kacTr]
    configureTracers trConfig docTChainSync           [tcsTr]
    configureTracers trConfig docTTxSubmission        [ttsTr]
    configureTracers trConfig docTStateQuery          [tsqTr]
    configureTracers trConfig docTChainSync           [tcsnTr]
    configureTracers trConfig docTChainSync           [tcssTr]
    configureTracers trConfig docTBlockFetch          [tbfTr]
    configureTracers trConfig docTBlockFetch          [tbfsTr]
    configureTracers trConfig docTTxSubmissionNode    [tsnTr]
    configureTracers trConfig docTTxSubmission2Node   [ts2nTr]
    configureTracers trConfig docIPSubscription       [ipsTr]
    configureTracers trConfig docDNSSubscription      [dnssTr]
    configureTracers trConfig docDNSResolver          [dnsrTr]
    configureTracers trConfig docErrorPolicy          [errpTr]
    configureTracers trConfig docLocalErrorPolicy     [lerrpTr]
    configureTracers trConfig docAcceptPolicy         [apTr]
    configureTracers trConfig docMux                  [muxTr]
    configureTracers trConfig docMux                  [muxLTr]
    configureTracers trConfig docHandshake            [hsTr]
    configureTracers trConfig docLocalHandshake       [lhsTr]
    configureTracers trConfig docDiffusionInit        [diTr]
    configureTracers trConfig docResourceStats        [rsTr]
    configureTracers trConfig docBasicInfo            [biTr]
    configureTracers trConfig docPeers                [pTr]

    mapM_ (traceWith biTr) basicInfos
    startResourceTracer
      rsTr
      (fromMaybe 1000 (tcResourceFreqency trConfig))
    startPeerTracer
      pTr
      nodeKernel
      (fromMaybe 2000 (tcPeerFreqency trConfig))

    replayBlockTracer <- withReplayedBlock rbTr
    let cdbmTr' = filterTrace
                      (\case (_, Just _, _) -> True
                             (_, Nothing, ChainDB.TraceLedgerReplayEvent
                                            (LedgerDB.ReplayedBlock {})) -> False
                             (_, Nothing, _) -> True)
                      cdbmTr

    pure Tracers
      { chainDBTracer = Tracer (traceWith cdbmTr')
                        <> Tracer (traceWith replayBlockTracer)
      , consensusTracers = Consensus.Tracers
        { Consensus.chainSyncClientTracer = Tracer (traceWith cscTr)
        , Consensus.chainSyncServerHeaderTracer = Tracer (traceWith csshTr)
        , Consensus.chainSyncServerBlockTracer = Tracer (traceWith cssbTr)
        , Consensus.blockFetchDecisionTracer = Tracer (traceWith bfdTr)
        , Consensus.blockFetchClientTracer = Tracer (traceWith bfcTr)

        , Consensus.blockFetchServerTracer = Tracer (traceWith bfsTr)
        , Consensus.forgeStateInfoTracer =
            Tracer (traceWith (traceAsKESInfo (Proxy @blk) fsiTr))
        , Consensus.txInboundTracer = Tracer (traceWith txiTr)
        , Consensus.txOutboundTracer = Tracer (traceWith txoTr)
        , Consensus.localTxSubmissionServerTracer = Tracer (traceWith ltxsTr)
        , Consensus.mempoolTracer = Tracer (traceWith mpTr)
        , Consensus.forgeTracer =
            Tracer (traceWith (contramap Left fTr))
            <> Tracer (traceWith (contramap Left fSttTr))
        , Consensus.blockchainTimeTracer = Tracer (traceWith btTr)
        , Consensus.keepAliveClientTracer = Tracer (traceWith kacTr)
        }
      , nodeToClientTracers = NtC.Tracers
        { NtC.tChainSyncTracer = Tracer (traceWith tcsTr)
        , NtC.tTxSubmissionTracer = Tracer (traceWith ttsTr)
        , NtC.tStateQueryTracer = Tracer (traceWith tsqTr)
        }
      , nodeToNodeTracers = NtN.Tracers
        { NtN.tChainSyncTracer = Tracer (traceWith tcsnTr)
        , NtN.tChainSyncSerialisedTracer = Tracer (traceWith tcssTr)
        , NtN.tBlockFetchTracer = Tracer (traceWith tbfTr)
        , NtN.tBlockFetchSerialisedTracer = Tracer (traceWith tbfsTr)
        , NtN.tTxSubmissionTracer = Tracer (traceWith tsnTr)
        , NtN.tTxSubmission2Tracer = Tracer (traceWith ts2nTr)
        }
      , ipSubscriptionTracer = Tracer (traceWith ipsTr)
      , dnsSubscriptionTracer= Tracer (traceWith dnssTr)
      , dnsResolverTracer = Tracer (traceWith dnsrTr)
      , errorPolicyTracer = Tracer (traceWith errpTr)
      , localErrorPolicyTracer = Tracer (traceWith lerrpTr)
      , acceptPolicyTracer = Tracer (traceWith apTr)
      , muxTracer = Tracer (traceWith muxTr)
      , muxLocalTracer = Tracer (traceWith muxLTr)
      , handshakeTracer = Tracer (traceWith hsTr)
      , localHandshakeTracer = Tracer (traceWith lhsTr)
      , diffusionInitializationTracer = Tracer (traceWith diTr)
      , basicInfoTracer = Tracer (traceWith biTr)
    }

mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect _ _ _ _ _ _ _ =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect

docTracers :: forall blk t.
  ( Show t
  , forall result. Show (Query blk result)
  , TraceConstraints blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , LedgerSupportsProtocol blk
  , Consensus.RunNode blk
  )
  => FilePath
  -> FilePath
  -> Proxy blk
  -> IO ()
docTracers configFileName outputFileName _ = do
    trConfig      <- readConfiguration configFileName
    let trBase    = docTracer (Stdout MachineFormat)
        trForward = docTracer Forwarder
        trDataPoint = docTracerDatapoint DatapointBackend
        mbTrEKG   = Just (docTracer EKGBackend)
    niTr <-   mkDataPointTracer
                trDataPoint
                (const ["NodeInfo"])
    cdbmTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                withAddedToCurrentChainEmptyLimited

    cscTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    csshTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    cssbTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    bfdTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    bfcTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    bfsTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    fsiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                allPublic
    txiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
    txoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
    ltxsTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    mpTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Mempool"
                namesForMempool
                severityMempool
                allPublic
    fTr    <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Forge"
                namesForForge
                severityForge
                allPublic
    -- fSttTr <- mkCardanoTracer' TODO JNF
    --             trBase trForward mbTrEKG
    --             "ForgeStats"
    --             namesForForge
    --             severityForge
    --             allPublic
    --             forgeThreadStats
    btTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    kacTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic

    tcsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
    ttsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
    tsqTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
    tcsnTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    tcssTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    tbfTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    tbfsTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    tsnTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
    ts2nTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    ipsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
    dnssTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
    dnsrTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
    errpTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
    lerrpTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
    apTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
    muxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Mux"
                namesForMux
                severityMux
                allPublic
    muxLTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
    hsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
    lhsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
    diTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
    rsTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Resources"
                namesForResources
                severityResources
                allPublic
    biTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BasicInfo"
                namesForBasicInfo
                severityBasicInfo
                allPublic
    pTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Peers"
                namesForPeers
                severityPeers
                allPublic

-- BasicInfo
    niTrDoc <- documentTracer trConfig niTr
      (docNodeInfoTraceEvent :: Documented NodeInfo)

-- ChainDB
    cdbmTrDoc <- documentTracer trConfig cdbmTr
      (docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk))

-- Consensus
    cscTrDoc <- documentTracer trConfig cscTr
        (docChainSyncClientEvent  :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceChainSyncClientEvent blk)))
    csshTrDoc <- documentTracer trConfig csshTr
        (docChainSyncServerEvent  :: Documented
          (TraceChainSyncServerEvent blk))
    cssbTrDoc <- documentTracer trConfig cssbTr
        (docChainSyncServerEvent  :: Documented
          (TraceChainSyncServerEvent blk))
    bfdTrDoc <- documentTracer trConfig bfdTr
        (docBlockFetchDecision :: Documented
           [BlockFetch.TraceLabelPeer Peer
             (FetchDecision [Point (Header blk)])])
    bfcTrDoc <- documentTracer trConfig bfcTr
        (docBlockFetchClient :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (BlockFetch.TraceFetchClientState (Header blk))))
    bfsTrDoc <- documentTracer trConfig bfsTr
        (docBlockFetchServer :: Documented
          (TraceBlockFetchServerEvent blk))
    fsiTrDoc <- documentTracer trConfig fsiTr
        (docForgeStateInfo :: Documented
           (Consensus.TraceLabelCreds HotKey.KESInfo))
    txiTrDoc <- documentTracer trConfig txiTr
        (docTxInbound :: Documented
           (BlockFetch.TraceLabelPeer Peer
             (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
    txoTrDoc <- documentTracer trConfig txoTr
        (docTxOutbound :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))
    ltxsTrDoc <- documentTracer trConfig ltxsTr
        (docLocalTxSubmissionServer :: Documented
          (TraceLocalTxSubmissionServerEvent blk))
    mpTrDoc <- documentTracer trConfig mpTr
        (docMempool :: Documented
          (TraceEventMempool blk))
    fTrDoc <- documentTracer trConfig fTr
        (docForge :: Documented
          (ForgeTracerType blk))
    -- fSttTrDoc <- documentTracer trConfig fSttTr TODO JNF
    --     (docForgeStats :: Documented
    --       ForgeThreadStats)
    btTrDoc <- documentTracer trConfig btTr
        (docBlockchainTime :: Documented
          (TraceBlockchainTimeEvent t))
    kacTrDoc <- documentTracer trConfig kacTr
        (docKeepAliveClient :: Documented
          (TraceKeepAliveClient Peer))

    tcsTrDoc <- documentTracer trConfig tcsTr
        (docTChainSync :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (ChainSync (Serialised blk) (Point blk) (Tip blk)))))
    ttsTrDoc <- documentTracer trConfig ttsTr
        (docTTxSubmission :: Documented
           (BlockFetch.TraceLabelPeer
              Peer
              (TraceSendRecv
                 (LTS.LocalTxSubmission
                    (GenTx blk) (ApplyTxErr blk)))))
    tsqTrDoc <- documentTracer trConfig tsqTr
        (docTStateQuery :: Documented
           (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (LocalStateQuery blk (Point blk) (Query blk)))))
    tcsnTrDoc <- documentTracer trConfig tcsnTr
        (docTChainSync :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (ChainSync (Header blk) (Point blk) (Tip blk)))))
    tcssTrDoc <- documentTracer trConfig tcssTr
        (docTChainSync :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))))
    tbfTrDoc <- documentTracer trConfig tbfTr
        (docTBlockFetch :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (BlockFetch blk (Point blk)))))
    tbfsTrDoc <- documentTracer trConfig tbfsTr
        (docTBlockFetch :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (BlockFetch (Serialised blk) (Point blk)))))
    tsnTrDoc <- documentTracer trConfig tsnTr
        (docTTxSubmissionNode :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (TxSubmission (GenTxId blk) (GenTx blk)))))
    ts2nTrDoc <- documentTracer trConfig ts2nTr
        (docTTxSubmission2Node :: Documented
          (BlockFetch.TraceLabelPeer Peer
            (TraceSendRecv
              (TxSubmission2 (GenTxId blk) (GenTx blk)))))
    ipsTrDoc <- documentTracer trConfig ipsTr
        (docIPSubscription :: Documented
          (WithIPList (SubscriptionTrace Socket.SockAddr)))
    dnssTrDoc <- documentTracer trConfig dnssTr
        (docDNSSubscription :: Documented
          (WithDomainName (SubscriptionTrace Socket.SockAddr)))
    dnsrTrDoc <- documentTracer trConfig dnsrTr
        (docDNSResolver :: Documented (WithDomainName DnsTrace))
    errpTrDoc <- documentTracer trConfig errpTr
        (docErrorPolicy :: Documented
          (WithAddr Socket.SockAddr ErrorPolicyTrace))
    lerrpTrDoc <- documentTracer trConfig lerrpTr
        (docLocalErrorPolicy :: Documented
          (WithAddr LocalAddress ErrorPolicyTrace))
    apTrDoc <- documentTracer trConfig apTr
        (docAcceptPolicy :: Documented
          NtN.AcceptConnectionsPolicyTrace)
    muxTrDoc <- documentTracer trConfig muxTr
        (docMux :: Documented
          (WithMuxBearer Peer MuxTrace))
    muxLTrDoc <- documentTracer trConfig muxLTr
        (docMux :: Documented
          (WithMuxBearer Peer MuxTrace))
    hsTrDoc <- documentTracer trConfig hsTr
        (docHandshake :: Documented NtN.HandshakeTr)
    lhsTrDoc <- documentTracer trConfig lhsTr
        (docLocalHandshake :: Documented NtC.HandshakeTr)
    diTrDoc <- documentTracer trConfig diTr
        (docDiffusionInit :: Documented ND.DiffusionInitializationTracer)

    rsTrDoc <- documentTracer trConfig rsTr
        (docResourceStats :: Documented ResourceStats)
    biTrDoc <- documentTracer trConfig biTr
        (docBasicInfo :: Documented BasicInfo)
    pTrDoc <- documentTracer trConfig pTr
        (docPeers :: Documented [PeerT blk])

    let bl = niTrDoc
            ++ cdbmTrDoc
            ++ cscTrDoc
            ++ csshTrDoc
            ++ cssbTrDoc
            ++ bfdTrDoc
            ++ bfcTrDoc
            ++ bfsTrDoc
            ++ fsiTrDoc
            ++ txiTrDoc
            ++ txoTrDoc
            ++ ltxsTrDoc
            ++ mpTrDoc
            ++ fTrDoc
            -- ++ fSttTrDoc
            ++ btTrDoc
            ++ kacTrDoc

            ++ tcsTrDoc
            ++ ttsTrDoc
            ++ tsqTrDoc
            ++ tcsnTrDoc
            ++ tcssTrDoc
            ++ tbfTrDoc
            ++ tbfsTrDoc
            ++ tsnTrDoc
            ++ ts2nTrDoc
            ++ ipsTrDoc
            ++ dnssTrDoc
            ++ dnsrTrDoc
            ++ errpTrDoc
            ++ lerrpTrDoc
            ++ apTrDoc
            ++ muxTrDoc
            ++ muxLTrDoc
            ++ hsTrDoc
            ++ lhsTrDoc
            ++ diTrDoc

            ++ rsTrDoc
            ++ biTrDoc
            ++ pTrDoc

    res <- buildersToText bl trConfig
    T.writeFile outputFileName res
    pure ()
