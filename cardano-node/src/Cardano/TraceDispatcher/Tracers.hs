{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-unused-imports  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  ) where

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Aeson.Types (ToJSON)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (trace)

import           Cardano.TraceDispatcher.Tracers.BlockReplayProgress
import           Cardano.TraceDispatcher.Tracers.ChainDB
import           Cardano.TraceDispatcher.Tracers.Consensus
import           Cardano.TraceDispatcher.Tracers.ForgingThreadStats
                     (ForgeThreadStats, docForgeStats, forgeThreadStats)
import           Cardano.TraceDispatcher.Tracers.KESInfo
import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Network.Combinators
import           Cardano.TraceDispatcher.Network.Docu
import           Cardano.TraceDispatcher.Network.Formatting ()
import           Cardano.TraceDispatcher.Tracers.P2P
import           Cardano.TraceDispatcher.Tracers.BasicInfo
import           Cardano.TraceDispatcher.Tracers.Peer
import           Cardano.TraceDispatcher.Tracers.Resources (namesForResources,
                     severityResources, startResourceTracer)
import qualified "trace-dispatcher" Control.Tracer as NT
import           Trace.Forward.Utils.DataPoint (DataPoint)

import           Cardano.Node.Configuration.Logging (EKGDirect)
import           Cardano.Node.Types (NodeInfo, docNodeInfoTraceEvent)

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.OrphanInstances.Common (ToObject)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..))

import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent)
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
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import           Ouroboros.Consensus.Node (NetworkP2PMode (..))
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
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion as ND
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.NodeToClient (LocalAddress,
                     NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..),
                     NodeToNodeVersion, RemoteAddress, WithAddr (..))
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

-- | Construct tracers for all system components.
--
mkDispatchTracers
  :: forall blk p2p.
  ( Consensus.RunNode blk
  , TraceConstraints blk

  , LogFormatting (LedgerEvent blk)
  , LogFormatting
    (BlockFetch.TraceLabelPeer
      (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
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
  -> NetworkP2PMode p2p
  -> IO (Tracers (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk p2p)
mkDispatchTracers _blockConfig (TraceDispatcher _trSel) _tr nodeKernel _ekgDirect
  trBase trForward mbTrEKG trDataPoint trConfig basicInfos nodeInfo enableP2P = do
    trace ("TraceConfig " <> show trConfig) $ pure ()

    -- Some special tracers
    -- NodeInfo tracer
    nodeInfoTr <- mkDataPointTracer
                trDataPoint
                (const ["NodeInfo"])
    traceWith nodeInfoTr nodeInfo

    -- Resource tracer
    resourcesTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Resources"
                (const [])
                (const Info)
                allPublic
    configureTracers trConfig docResourceStats        [resourcesTr]
    startResourceTracer
      resourcesTr
      (fromMaybe 1000 (tcResourceFreqency trConfig))

    -- BasicInfo tracer
    basicInfoTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BasicInfo"
                namesForBasicInfo
                severityBasicInfo
                allPublic
    configureTracers trConfig docBasicInfo [basicInfoTr]
    mapM_ (traceWith basicInfoTr) basicInfos

    -- Peers tracer
    peersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Peers"
                namesForPeers
                severityPeers
                allPublic
    configureTracers trConfig docPeers [peersTr]
    startPeerTracer
      peersTr
      nodeKernel
      (fromMaybe 2000 (tcPeerFreqency trConfig))


    chainDBTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                withAddedToCurrentChainEmptyLimited
    configureTracers trConfig docChainDBTraceEvent [chainDBTr]
    replayBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ReplayBlock"
                namesForReplayBlockStats
                severityReplayBlockStats
                allPublic
    configureTracers trConfig docReplayedBlock [replayBlockTr]
    replayBlockTr2 <- withReplayedBlock replayBlockTr
    let chainDBTr2 = filterTrace
                      (\case (_, Just _, _) -> True
                             (_, Nothing, ChainDB.TraceLedgerReplayEvent
                                            (LedgerDB.ReplayedBlock {})) -> False
                             (_, Nothing, _) -> True)
                      chainDBTr

    consensusTr :: Consensus.Tracers
                    IO
                    (ConnectionId RemoteAddress)
                    (ConnectionId LocalAddress)
                    blk <-
      mkConsensusTracers trBase trForward mbTrEKG trDataPoint trConfig nodeKernel
    nodeToClientTr :: NodeToClient.Tracers
                    IO
                    (ConnectionId LocalAddress)
                    blk
                    DeserialiseFailure <-
      mkNodeToClientTracers trBase trForward mbTrEKG trDataPoint trConfig
    nodeToNodeTr :: NodeToNode.Tracers
                    IO
                    (ConnectionId RemoteAddress)
                    blk
                    DeserialiseFailure <-
      mkNodeToNodeTracers trBase trForward mbTrEKG trDataPoint trConfig
    diffusionTr :: Diffusion.Tracers
                    RemoteAddress
                    NodeToNodeVersion
                    LocalAddress
                    NodeToClientVersion
                    IO <-
      mkDiffusionTracers trBase trForward mbTrEKG trDataPoint trConfig
    diffusionTrExtra :: Diffusion.ExtraTracers p2p <-
      mkDiffusionTracersExtra trBase trForward mbTrEKG trDataPoint trConfig enableP2P
    pure Tracers
      { chainDBTracer = Tracer (traceWith chainDBTr2)
                        <> Tracer (traceWith replayBlockTr2)
      , consensusTracers = consensusTr
      , nodeToClientTracers = nodeToClientTr
      , nodeToNodeTracers = nodeToNodeTr
      , diffusionTracers = diffusionTr
      , diffusionTracersExtra = diffusionTrExtra
      , startupTracer = mempty
    }

mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect _ _ _ _ _ _ _ enableP2P =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect enableP2P

mkConsensusTracers :: forall blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , TraceConstraints blk
  , LogFormatting (BlockFetch.TraceLabelPeer
                    (ConnectionId RemoteAddress) (TraceChainSyncClientEvent blk))
  )
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NodeKernelData blk
  -> IO (Consensus.Tracers IO (ConnectionId RemoteAddress) (ConnectionId LocalAddress) blk)
mkConsensusTracers trBase trForward mbTrEKG _trDataPoint trConfig nodeKernel = do
    chainSyncClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    configureTracers trConfig docChainSyncClientEvent [chainSyncClientTr]
    chainSyncServerHeaderTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEvent [chainSyncServerHeaderTr]
    chainSyncServerBlockTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    configureTracers trConfig docChainSyncServerEvent [chainSyncServerBlockTr]
    blockFetchDecisionTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    configureTracers trConfig docBlockFetchDecision [blockFetchDecisionTr]
    blockFetchClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    configureTracers trConfig docBlockFetchClient [blockFetchClientTr]
    blockFetchServerTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    configureTracers trConfig docBlockFetchServer [blockFetchServerTr]
    forgeKESInfoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ForgeStateInfo"
                namesForKESInfo
                severityKESInfo
                allPublic
    configureTracers trConfig docForgeKESInfo [forgeKESInfoTr]
    txInboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
    configureTracers trConfig docTxInbound [txInboundTr]
    txOutboundTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
    configureTracers trConfig docTxOutbound [txOutboundTr]
    localTxSubmissionServerTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    configureTracers trConfig docLocalTxSubmissionServer [localTxSubmissionServerTr]
    mempoolTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Mempool"
                namesForMempool
                severityMempool
                allPublic
    configureTracers trConfig docMempool [mempoolTr]
    forgeTr    <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "Forge"
                namesForForge
                severityForge
                allPublic
                (forgeTracerTransform nodeKernel)
    forgeThreadStatsTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                forgeThreadStats
    configureTracers trConfig docForge [forgeTr, forgeThreadStatsTr]
    blockchainTimeTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    configureTracers trConfig docBlockchainTime [blockchainTimeTr]
    keepAliveClientTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
    configureTracers trConfig docKeepAliveClient [keepAliveClientTr]
    pure $ Consensus.Tracers
      { Consensus.chainSyncClientTracer = Tracer $
          traceWith chainSyncClientTr
      , Consensus.chainSyncServerHeaderTracer = Tracer $
          traceWith chainSyncServerHeaderTr
      , Consensus.chainSyncServerBlockTracer = Tracer $
          traceWith chainSyncServerBlockTr
      , Consensus.blockFetchDecisionTracer = Tracer $
          traceWith blockFetchDecisionTr
      , Consensus.blockFetchClientTracer = Tracer $
          traceWith blockFetchClientTr
      , Consensus.blockFetchServerTracer = Tracer $
          traceWith blockFetchServerTr
      , Consensus.forgeStateInfoTracer = Tracer $
          traceWith (traceAsKESInfo (Proxy @blk) forgeKESInfoTr)
      , Consensus.txInboundTracer = Tracer $
          traceWith txInboundTr
      , Consensus.txOutboundTracer = Tracer $
          traceWith txOutboundTr
      , Consensus.localTxSubmissionServerTracer = Tracer $
          traceWith localTxSubmissionServerTr
      , Consensus.mempoolTracer = Tracer $
          traceWith mempoolTr
      , Consensus.forgeTracer =
          Tracer (traceWith (contramap Left forgeTr))
          <> Tracer (traceWith (contramap Left forgeThreadStatsTr))
      , Consensus.blockchainTimeTracer = Tracer $
          traceWith blockchainTimeTr
      , Consensus.keepAliveClientTracer = Tracer $
          traceWith keepAliveClientTr
      }

mkNodeToClientTracers :: forall blk.
  ( Consensus.RunNode blk
  )
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (NodeToClient.Tracers IO (ConnectionId LocalAddress) blk DeserialiseFailure)
mkNodeToClientTracers trBase trForward mbTrEKG _trDataPoint trConfig = do
    chainSyncTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
    configureTracers trConfig docTChainSync [chainSyncTr]
    txSubmissionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
    configureTracers trConfig docTTxSubmission [txSubmissionTr]
    stateQueryTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
    configureTracers trConfig docTStateQuery [stateQueryTr]
    pure $ NtC.Tracers
      { NtC.tChainSyncTracer = Tracer $
          traceWith chainSyncTr
      , NtC.tTxSubmissionTracer = Tracer $
          traceWith txSubmissionTr
      , NtC.tStateQueryTracer = Tracer $
          traceWith stateQueryTr
      }

mkNodeToNodeTracers :: forall blk.
  ( Consensus.RunNode blk
  , TraceConstraints blk)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (NodeToNode.Tracers IO (ConnectionId RemoteAddress) blk DeserialiseFailure)
mkNodeToNodeTracers trBase trForward mbTrEKG _trDataPoint trConfig = do
    chainSyncTracer <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    configureTracers trConfig docTChainSync [chainSyncTracer]
    chainSyncSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    configureTracers trConfig docTChainSync [chainSyncSerialisedTr]
    blockFetchTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchTr]
    blockFetchSerialisedTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    configureTracers trConfig docTBlockFetch [blockFetchSerialisedTr]
    txSubmissionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
    configureTracers trConfig docTTxSubmissionNode [txSubmissionTr]
    txSubmission2Tracer  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    configureTracers trConfig docTTxSubmission2Node [txSubmission2Tracer]
    pure $ NtN.Tracers
      { NtN.tChainSyncTracer = Tracer $
          traceWith chainSyncTracer
      , NtN.tChainSyncSerialisedTracer = Tracer $
          traceWith chainSyncSerialisedTr
      , NtN.tBlockFetchTracer = Tracer $
          traceWith blockFetchTr
      , NtN.tBlockFetchSerialisedTracer = Tracer $
          traceWith blockFetchSerialisedTr
      , NtN.tTxSubmissionTracer = Tracer $
          traceWith txSubmissionTr
      , NtN.tTxSubmission2Tracer = Tracer $
          traceWith txSubmission2Tracer
      }

mkDiffusionTracers ::
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> IO (Diffusion.Tracers RemoteAddress NodeToNodeVersion
        LocalAddress NodeToClientVersion IO)
mkDiffusionTracers  trBase trForward mbTrEKG _trDataPoint trConfig = do
    dtMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Mux"
                namesForMux
                severityMux
                allPublic
    configureTracers trConfig docMux [dtMuxTr]
    dtLocalMuxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
    configureTracers trConfig docMux [dtLocalMuxTr]
    dtHandshakeTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
    configureTracers trConfig docHandshake [dtHandshakeTr]
    dtLocalHandshakeTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
    configureTracers trConfig docLocalHandshake [dtLocalHandshakeTr]
    dtDiffusionInitializationTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
    configureTracers trConfig docDiffusionInit [dtDiffusionInitializationTr]
    dtLedgerPeersTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LedgerPeers"
                namesForLedgerPeers
                severityLedgerPeers
                allPublic
    configureTracers trConfig docLedgerPeers [dtLedgerPeersTr]
    pure $ Diffusion.Tracers
       { Diffusion.dtMuxTracer                     = Tracer $
           traceWith dtMuxTr
       , Diffusion.dtHandshakeTracer               = Tracer $
           traceWith dtHandshakeTr
       , Diffusion.dtLocalMuxTracer                = Tracer $
           traceWith dtLocalMuxTr
       , Diffusion.dtLocalHandshakeTracer          = Tracer $
           traceWith dtLocalHandshakeTr
       , Diffusion.dtDiffusionInitializationTracer = Tracer $
           traceWith dtDiffusionInitializationTr
       , Diffusion.dtLedgerPeersTracer             = Tracer $
           traceWith dtLedgerPeersTr
       }

mkDiffusionTracersExtra  :: forall p2p.
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Trace IO DataPoint
  -> TraceConfig
  -> NetworkP2PMode p2p
  -> IO (Diffusion.ExtraTracers p2p)
mkDiffusionTracersExtra trBase trForward mbTrEKG _trDataPoint trConfig EnabledP2PMode = do
    localRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalRootPeers"
      namesForLocalRootPeers
      severityLocalRootPeers
      allPublic
    configureTracers trConfig docLocalRootPeers [localRootPeersTr]
    publicRootPeersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PublicRootPeers"
      namesForPublicRootPeers
      severityPublicRootPeers
      allPublic
    configureTracers trConfig docPublicRootPeers [publicRootPeersTr]
    peerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PeerSelection"
      namesForPeerSelection
      severityPeerSelection
      allPublic
    configureTracers trConfig docPeerSelection [peerSelectionTr]
    debugPeerSelectionTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "DebugPeerSelection"
      namesForDebugPeerSelection
      severityDebugPeerSelection
      allPublic
    configureTracers trConfig docDebugPeerSelection [debugPeerSelectionTr]
    debugPeerSelectionResponderTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "DebugPeerSelectionResponder"
      namesForDebugPeerSelection
      severityDebugPeerSelection
      allPublic
    configureTracers trConfig docDebugPeerSelection [debugPeerSelectionResponderTr]
    peerSelectionCountersTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PeerSelectionCounters"
      namesForPeerSelectionCounters
      severityPeerSelectionCounters
      allPublic
    configureTracers trConfig docPeerSelectionCounters [peerSelectionCountersTr]
    peerSelectionActionsTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "PeerSelectionActions"
      namesForPeerSelectionActions
      severityPeerSelectionActions
      allPublic
    configureTracers trConfig docPeerSelectionActions [peerSelectionActionsTr]
    connectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "ConnectionManager"
      namesForConnectionManager
      severityConnectionManager
      allPublic
    configureTracers trConfig docConnectionManager [connectionManagerTr]
    serverTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "Server"
      namesForServer
      severityServer
      allPublic
    configureTracers trConfig docServer [serverTr]
    inboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "InboundGovernor"
      namesForInboundGovernor
      severityInboundGovernor
      allPublic
    configureTracers trConfig docInboundGovernor [inboundGovernorTr]
    localConnectionManagerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalConnectionManager"
      namesForConnectionManager
      severityConnectionManager
      allPublic
    configureTracers trConfig docConnectionManager [localConnectionManagerTr]
    localServerTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalServer"
      namesForServer
      severityServer
      allPublic
    configureTracers trConfig docServer [localServerTr]
    localInboundGovernorTr  <-  mkCardanoTracer
      trBase trForward mbTrEKG
      "LocalInboundGovernor"
      namesForInboundGovernor
      severityInboundGovernor
      allPublic
    configureTracers trConfig docInboundGovernor [localInboundGovernorTr]
    pure $ Diffusion.P2PTracers P2P.TracersExtra
             { P2P.dtTraceLocalRootPeersTracer = Tracer $
                 traceWith localRootPeersTr
             , P2P.dtTracePublicRootPeersTracer = Tracer $
                 traceWith publicRootPeersTr
             , P2P.dtTracePeerSelectionTracer = Tracer $
                 traceWith peerSelectionTr
             , P2P.dtDebugPeerSelectionInitiatorTracer = Tracer $
                 traceWith debugPeerSelectionTr
             , P2P.dtDebugPeerSelectionInitiatorResponderTracer = Tracer $
                 traceWith debugPeerSelectionResponderTr
             , P2P.dtTracePeerSelectionCounters = Tracer $
                 traceWith peerSelectionCountersTr
             , P2P.dtPeerSelectionActionsTracer = Tracer $
                 traceWith peerSelectionActionsTr
             , P2P.dtConnectionManagerTracer = Tracer $
                 traceWith connectionManagerTr
             , P2P.dtServerTracer = Tracer $
                 traceWith serverTr
             , P2P.dtInboundGovernorTracer = Tracer $
                 traceWith inboundGovernorTr
             , P2P.dtLocalConnectionManagerTracer =  Tracer $
                 traceWith localConnectionManagerTr
             , P2P.dtLocalServerTracer = Tracer $
                 traceWith localServerTr
             , P2P.dtLocalInboundGovernorTracer = Tracer $
                 traceWith localInboundGovernorTr
             }

mkDiffusionTracersExtra trBase trForward mbTrEKG _trDataPoint trConfig DisabledP2PMode = do
    dtIpSubscriptionTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
    configureTracers trConfig docIPSubscription [dtIpSubscriptionTr]
    dtDnsSubscriptionTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
    configureTracers trConfig docDNSSubscription [dtDnsSubscriptionTr]
    dtDnsResolverTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
    configureTracers trConfig docDNSResolver [dtDnsResolverTr]
    dtErrorPolicyTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
    configureTracers trConfig docErrorPolicy [dtErrorPolicyTr]
    dtLocalErrorPolicyTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
    configureTracers trConfig docLocalErrorPolicy [dtLocalErrorPolicyTr]
    dtAcceptPolicyTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
    configureTracers trConfig docAcceptPolicy [dtAcceptPolicyTr]
    pure $ Diffusion.NonP2PTracers NonP2P.TracersExtra
       { NonP2P.dtIpSubscriptionTracer = Tracer $
           traceWith dtIpSubscriptionTr
       , NonP2P.dtDnsSubscriptionTracer = Tracer $
           traceWith dtDnsSubscriptionTr
       , NonP2P.dtDnsResolverTracer = Tracer $
           traceWith dtDnsResolverTr
       , NonP2P.dtErrorPolicyTracer = Tracer $
           traceWith dtErrorPolicyTr
       , NonP2P.dtLocalErrorPolicyTracer = Tracer $
           traceWith dtLocalErrorPolicyTr
       , NonP2P.dtAcceptPolicyTracer = Tracer $
           traceWith dtAcceptPolicyTr
       }


-- docTracers :: forall blk t.
--   ( Show t
--   , forall result. Show (Query blk result)
--   , TraceConstraints blk
--   , LogFormatting (ChainDB.InvalidBlockReason blk)
--   , LedgerSupportsProtocol blk
--   , Consensus.RunNode blk
--   )
--   => FilePath
--   -> FilePath
--   -> Proxy blk
--   -> IO ()
-- docTracers _configFileName _outputFileName _ = undefined -- do
--     trConfig      <- readConfiguration configFileName
--     let trBase    = docTracer (Stdout MachineFormat)
--         trForward = docTracer Forwarder
--         trDataPoint = docTracerDatapoint DatapointBackend
--         mbTrEKG   = Just (docTracer EKGBackend)
--     niTr <-   mkDataPointTracer
--                 trDataPoint
--                 (const ["NodeInfo"])
--     cdbmTr <- mkCardanoTracer'
--                 trBase trForward mbTrEKG
--                 "ChainDB"
--                 namesForChainDBTraceEvents
--                 severityChainDB
--                 allPublic
--                 withAddedToCurrentChainEmptyLimited
--
--     cscTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ChainSyncClient"
--                 namesForChainSyncClientEvent
--                 severityChainSyncClientEvent
--                 allPublic
--     csshTr <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ChainSyncServerHeader"
--                 namesForChainSyncServerEvent
--                 severityChainSyncServerEvent
--                 allPublic
--     cssbTr <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ChainSyncServerBlock"
--                 namesForChainSyncServerEvent
--                 severityChainSyncServerEvent
--                 allPublic
--     bfdTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "BlockFetchDecision"
--                 namesForBlockFetchDecision
--                 severityBlockFetchDecision
--                 allConfidential
--     bfcTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "BlockFetchClient"
--                 namesForBlockFetchClient
--                 severityBlockFetchClient
--                 allPublic
--     bfsTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "BlockFetchServer"
--                 namesForBlockFetchServer
--                 severityBlockFetchServer
--                 allPublic
--     fsiTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ForgeStateInfo"
--                 namesForStateInfo
--                 severityStateInfo
--                 allPublic
--     txiTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "TxInbound"
--                 namesForTxInbound
--                 severityTxInbound
--                 allPublic
--     txoTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "TxOutbound"
--                 namesForTxOutbound
--                 severityTxOutbound
--                 allPublic
--     ltxsTr <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "LocalTxSubmissionServer"
--                 namesForLocalTxSubmissionServer
--                 severityLocalTxSubmissionServer
--                 allPublic
--     mpTr   <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Mempool"
--                 namesForMempool
--                 severityMempool
--                 allPublic
--     fTr    <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Forge"
--                 namesForForge
--                 severityForge
--                 allPublic
--     -- fSttTr <- mkCardanoTracer' TODO JNF
--     --             trBase trForward mbTrEKG
--     --             "ForgeStats"
--     --             namesForForge
--     --             severityForge
--     --             allPublic
--     --             forgeThreadStats
--     btTr   <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "BlockchainTime"
--                 namesForBlockchainTime
--                 severityBlockchainTime
--                 allPublic
--     kacTr  <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "KeepAliveClient"
--                 namesForKeepAliveClient
--                 severityKeepAliveClient
--                 allPublic
--
--     tcsTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ChainSyncClient"
--                 namesForTChainSync
--                 severityTChainSync
--                 allPublic
--     ttsTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "TxSubmissionClient"
--                 namesForTTxSubmission
--                 severityTTxSubmission
--                 allPublic
--     tsqTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "StateQueryClient"
--                 namesForTStateQuery
--                 severityTStateQuery
--                 allPublic
--     tcsnTr <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ChainSyncNode"
--                 namesForTChainSyncNode
--                 severityTChainSyncNode
--                 allPublic
--     tcssTr <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ChainSyncSerialised"
--                 namesForTChainSyncSerialised
--                 severityTChainSyncSerialised
--                 allPublic
--     tbfTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "BlockFetch"
--                 namesForTBlockFetch
--                 severityTBlockFetch
--                 allPublic
--     tbfsTr <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "BlockFetchSerialised"
--                 namesForTBlockFetchSerialised
--                 severityTBlockFetchSerialised
--                 allPublic
--     tsnTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "TxSubmission"
--                 namesForTxSubmissionNode
--                 severityTxSubmissionNode
--                 allPublic
--     ts2nTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "TxSubmission2"
--                 namesForTxSubmission2Node
--                 severityTxSubmission2Node
--                 allPublic
--     ipsTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "IpSubscription"
--                 namesForIPSubscription
--                 severityIPSubscription
--                 allPublic
--     dnssTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "DNSSubscription"
--                 namesForDNSSubscription
--                 severityDNSSubscription
--                 allPublic
--     dnsrTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "DNSResolver"
--                 namesForDNSResolver
--                 severityDNSResolver
--                 allPublic
--     errpTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "ErrorPolicy"
--                 namesForErrorPolicy
--                 severityErrorPolicy
--                 allPublic
--     lerrpTr <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "LocalErrorPolicy"
--                 namesForLocalErrorPolicy
--                 severityLocalErrorPolicy
--                 allPublic
--     apTr    <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "AcceptPolicy"
--                 namesForAcceptPolicy
--                 severityAcceptPolicy
--                 allPublic
--     muxTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Mux"
--                 namesForMux
--                 severityMux
--                 allPublic
--     muxLTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "MuxLocal"
--                 namesForMux
--                 severityMux
--                 allPublic
--     hsTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Handshake"
--                 namesForHandshake
--                 severityHandshake
--                 allPublic
--     lhsTr  <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "LocalHandshake"
--                 namesForLocalHandshake
--                 severityLocalHandshake
--                 allPublic
--     diTr   <-  mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "DiffusionInit"
--                 namesForDiffusionInit
--                 severityDiffusionInit
--                 allPublic
--     rsTr   <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Resources"
--                 namesForResources
--                 severityResources
--                 allPublic
--     biTr   <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "BasicInfo"
--                 namesForBasicInfo
--                 severityBasicInfo
--                 allPublic
--     pTr   <- mkCardanoTracer
--                 trBase trForward mbTrEKG
--                 "Peers"
--                 namesForPeers
--                 severityPeers
--                 allPublic
--
-- -- BasicInfo
--     niTrDoc <- documentTracer trConfig niTr
--       (docNodeInfoTraceEvent :: Documented NodeInfo)
--
-- -- ChainDB
--     cdbmTrDoc <- documentTracer trConfig cdbmTr
--       (docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk))
--
-- -- Consensus
--     cscTrDoc <- documentTracer trConfig cscTr
--         (docChainSyncClientEvent  :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceChainSyncClientEvent blk)))
--     csshTrDoc <- documentTracer trConfig csshTr
--         (docChainSyncServerEvent  :: Documented
--           (TraceChainSyncServerEvent blk))
--     cssbTrDoc <- documentTracer trConfig cssbTr
--         (docChainSyncServerEvent  :: Documented
--           (TraceChainSyncServerEvent blk))
--     bfdTrDoc <- documentTracer trConfig bfdTr
--         (docBlockFetchDecision :: Documented
--            [BlockFetch.TraceLabelPeer Peer
--              (FetchDecision [Point (Header blk)])])
--     bfcTrDoc <- documentTracer trConfig bfcTr
--         (docBlockFetchClient :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (BlockFetch.TraceFetchClientState (Header blk))))
--     bfsTrDoc <- documentTracer trConfig bfsTr
--         (docBlockFetchServer :: Documented
--           (TraceBlockFetchServerEvent blk))
--     fsiTrDoc <- documentTracer trConfig fsiTr
--         (docForgeStateInfo :: Documented
--            (Consensus.TraceLabelCreds HotKey.KESInfo))
--     txiTrDoc <- documentTracer trConfig txiTr
--         (docTxInbound :: Documented
--            (BlockFetch.TraceLabelPeer Peer
--              (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
--     txoTrDoc <- documentTracer trConfig txoTr
--         (docTxOutbound :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))
--     ltxsTrDoc <- documentTracer trConfig ltxsTr
--         (docLocalTxSubmissionServer :: Documented
--           (TraceLocalTxSubmissionServerEvent blk))
--     mpTrDoc <- documentTracer trConfig mpTr
--         (docMempool :: Documented
--           (TraceEventMempool blk))
--     fTrDoc <- documentTracer trConfig fTr
--         (docForge :: Documented
--           (ForgeTracerType blk))
--     -- fSttTrDoc <- documentTracer trConfig fSttTr TODO JNF
--     --     (docForgeStats :: Documented
--     --       ForgeThreadStats)
--     btTrDoc <- documentTracer trConfig btTr
--         (docBlockchainTime :: Documented
--           (TraceBlockchainTimeEvent t))
--     kacTrDoc <- documentTracer trConfig kacTr
--         (docKeepAliveClient :: Documented
--           (TraceKeepAliveClient Peer))
--
--     tcsTrDoc <- documentTracer trConfig tcsTr
--         (docTChainSync :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (ChainSync (Serialised blk) (Point blk) (Tip blk)))))
--     ttsTrDoc <- documentTracer trConfig ttsTr
--         (docTTxSubmission :: Documented
--            (BlockFetch.TraceLabelPeer
--               Peer
--               (TraceSendRecv
--                  (LTS.LocalTxSubmission
--                     (GenTx blk) (ApplyTxErr blk)))))
--     tsqTrDoc <- documentTracer trConfig tsqTr
--         (docTStateQuery :: Documented
--            (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (LocalStateQuery blk (Point blk) (Query blk)))))
--     tcsnTrDoc <- documentTracer trConfig tcsnTr
--         (docTChainSync :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (ChainSync (Header blk) (Point blk) (Tip blk)))))
--     tcssTrDoc <- documentTracer trConfig tcssTr
--         (docTChainSync :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))))
--     tbfTrDoc <- documentTracer trConfig tbfTr
--         (docTBlockFetch :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (BlockFetch blk (Point blk)))))
--     tbfsTrDoc <- documentTracer trConfig tbfsTr
--         (docTBlockFetch :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (BlockFetch (Serialised blk) (Point blk)))))
--     tsnTrDoc <- documentTracer trConfig tsnTr
--         (docTTxSubmissionNode :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (TxSubmission (GenTxId blk) (GenTx blk)))))
--     ts2nTrDoc <- documentTracer trConfig ts2nTr
--         (docTTxSubmission2Node :: Documented
--           (BlockFetch.TraceLabelPeer Peer
--             (TraceSendRecv
--               (TxSubmission2 (GenTxId blk) (GenTx blk)))))
--     ipsTrDoc <- documentTracer trConfig ipsTr
--         (docIPSubscription :: Documented
--           (WithIPList (SubscriptionTrace Socket.SockAddr)))
--     dnssTrDoc <- documentTracer trConfig dnssTr
--         (docDNSSubscription :: Documented
--           (WithDomainName (SubscriptionTrace Socket.SockAddr)))
--     dnsrTrDoc <- documentTracer trConfig dnsrTr
--         (docDNSResolver :: Documented (WithDomainName DnsTrace))
--     errpTrDoc <- documentTracer trConfig errpTr
--         (docErrorPolicy :: Documented
--           (WithAddr Socket.SockAddr ErrorPolicyTrace))
--     lerrpTrDoc <- documentTracer trConfig lerrpTr
--         (docLocalErrorPolicy :: Documented
--           (WithAddr LocalAddress ErrorPolicyTrace))
--     apTrDoc <- documentTracer trConfig apTr
--         (docAcceptPolicy :: Documented
--           NtN.AcceptConnectionsPolicyTrace)
--     muxTrDoc <- documentTracer trConfig muxTr
--         (docMux :: Documented
--           (WithMuxBearer Peer MuxTrace))
--     muxLTrDoc <- documentTracer trConfig muxLTr
--         (docMux :: Documented
--           (WithMuxBearer Peer MuxTrace))
--     hsTrDoc <- documentTracer trConfig hsTr
--         (docHandshake :: Documented NtN.HandshakeTr)
--     lhsTrDoc <- documentTracer trConfig lhsTr
--         (docLocalHandshake :: Documented NtC.HandshakeTr)
--     diTrDoc <- documentTracer trConfig diTr
--         (docDiffusionInit :: Documented ND.DiffusionInitializationTracer)
--
--     rsTrDoc <- documentTracer trConfig rsTr
--         (docResourceStats :: Documented ResourceStats)
--     biTrDoc <- documentTracer trConfig biTr
--         (docBasicInfo :: Documented BasicInfo)
--     pTrDoc <- documentTracer trConfig pTr
--         (docPeers :: Documented [PeerT blk])
--
--     let bl = niTrDoc
--             ++ cdbmTrDoc
--             ++ cscTrDoc
--             ++ csshTrDoc
--             ++ cssbTrDoc
--             ++ bfdTrDoc
--             ++ bfcTrDoc
--             ++ bfsTrDoc
--             ++ fsiTrDoc
--             ++ txiTrDoc
--             ++ txoTrDoc
--             ++ ltxsTrDoc
--             ++ mpTrDoc
--             ++ fTrDoc
--             -- ++ fSttTrDoc
--             ++ btTrDoc
--             ++ kacTrDoc
--
--             ++ tcsTrDoc
--             ++ ttsTrDoc
--             ++ tsqTrDoc
--             ++ tcsnTrDoc
--             ++ tcssTrDoc
--             ++ tbfTrDoc
--             ++ tbfsTrDoc
--             ++ tsnTrDoc
--             ++ ts2nTrDoc
--             ++ ipsTrDoc
--             ++ dnssTrDoc
--             ++ dnsrTrDoc
--             ++ errpTrDoc
--             ++ lerrpTrDoc
--             ++ apTrDoc
--             ++ muxTrDoc
--             ++ muxLTrDoc
--             ++ hsTrDoc
--             ++ lhsTrDoc
--             ++ diTrDoc
--
--             ++ rsTrDoc
--             ++ biTrDoc
--             ++ pTrDoc
--
--     res <- buildersToText bl trConfig
--     T.writeFile outputFileName res
--     pure ()
