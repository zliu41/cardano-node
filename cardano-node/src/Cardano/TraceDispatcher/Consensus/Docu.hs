{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Consensus.Docu
  ( docChainSyncClientEvent
  , docChainSyncServerEvent
  , docBlockFetchDecision
  , docBlockFetchClient
  , docBlockFetchServer
  , docTxInbound
  , docTxOutbound
  , docLocalTxSubmissionServer
  , docMempool
  , docForge
  , docForgeStateInfo
  , docBlockchainTime
  , docKeepAliveClient
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Mempool.API (
                     TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Cardano.TraceDispatcher.Consensus.Combinators
                     (TraceStartLeadershipCheckPlus (..))
import           Cardano.TraceDispatcher.Era.Byron ()
import           Cardano.TraceDispatcher.Era.Shelley ()


docChainSyncClientEvent ::
  Documented (BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk))
docChainSyncClientEvent = Documented [
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceDownloadedHeader anyProto))
      []
      "While following a candidate chain, we rolled forward by downloading a\
      \ header."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceRolledBack anyProto))
      []
      "While following a candidate chain, we rolled back to the given point."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceFoundIntersection anyProto anyProto anyProto))
      []
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceException anyProto))
      []
      "An exception was thrown by the Chain Sync Client."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceTermination anyProto))
      []
      "The client has terminated."
  ]

docChainSyncServerEvent :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEvent = Documented [
    DocMsg
      (TraceChainSyncServerRead anyProto anyProto)
      []
      "A server read has occured, either for an add block or a rollback"
    , DocMsg
      (TraceChainSyncServerReadBlocked anyProto anyProto)
      []
      "A server read has blocked, either for an add block or a rollback"
    , DocMsg
      (TraceChainSyncRollForward anyProto)
      [("cardano.node.chainSync.rollForward", "TODO TraceDoc")]
      "Roll forward to the given point."
    , DocMsg
      (TraceChainSyncRollBackward anyProto)
      []
      "TODO TraceDoc"
  ]

docBlockFetchDecision ::
  Documented [BlockFetch.TraceLabelPeer remotePeer (FetchDecision [Point (Header blk)])]
docBlockFetchDecision = Documented [
    DocMsg
      [BlockFetch.TraceLabelPeer anyProto (Right anyProto)]
      [("cardano.node.connectedPeers", "Number of connected peers")]
      "Throughout the decision making process we accumulate reasons to decline\
      \ to fetch any blocks. This message carries the intermediate and final\
      \ results."
  ]


docBlockFetchClient ::
  Documented (BlockFetch.TraceLabelPeer remotePeer (BlockFetch.TraceFetchClientState (Header blk)))
docBlockFetchClient = Documented [
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.AddedFetchRequest
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "The block fetch decision thread has added a new fetch instruction\
      \ consisting of one or more individual request ranges."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.AcknowledgedFetchRequest
          anyProto))
      []
      "Mark the point when the fetch client picks up the request added\
      \ by the block fetch decision thread. Note that this event can happen\
      \ fewer times than the 'AddedFetchRequest' due to fetch request merging."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.StartedFetchBatch
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "Mark the start of receiving a streaming batch of blocks. This will\
      \ be followed by one or more 'CompletedBlockFetch' and a final\
      \ 'CompletedFetchBatch'"
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.CompletedFetchBatch
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "Mark the successful end of receiving a streaming batch of blocks."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.RejectedFetchBatch
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "If the other peer rejects our request then we have this event\
      \ instead of 'StartedFetchBatch' and 'CompletedFetchBatch'."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.ClientTerminating 1))
      []
      "The client is terminating.  Log the number of outstanding\
      \ requests."
  ]

docBlockFetchServer ::
  Documented (TraceBlockFetchServerEvent blk)
docBlockFetchServer = Documented [
    DocMsg
      (TraceBlockFetchServerSendBlock GenesisPoint)
      [("cardano.node.served.block", "TODO TraceDoc")]
      "The server sent a block to the peer."
  ]


docTxInbound ::
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionInbound txid tx))
docTxInbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionCollected 1))
    [ ("cardano.node.submissions.submitted", "TODO a")]
    "Number of transactions just about to be inserted."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionProcessed (ProcessedTxCount 1 2)))
    [ ("cardano.node.submissions.accepted", "TODO TraceDoc")
    , ("cardano.node.submissions.rejected", "TODO TraceDoc")
    ]
    "Just processed transaction pass/fail breakdown."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      TraceTxInboundTerminated)
    []
    "Server received 'MsgDone'."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxInboundCanRequestMoreTxs 1))
    []
    "There are no replies in flight, but we do know some more txs we\
    \ can ask for, so lets ask for them and more txids."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxInboundCannotRequestMoreTxs 1))
    []
    "There's no replies in flight, and we have no more txs we can\
    \ ask for so the only remaining thing to do is to ask for more\
    \ txids. Since this is the only thing to do now, we make this a\
    \ blocking call."
  ]

docTxOutbound :: forall remotePeer txid tx.
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionOutbound txid tx))
docTxOutbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionOutboundRecvMsgRequestTxs anyProto))
    []
    "The IDs of the transactions requested."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionOutboundSendMsgReplyTxs anyProto))
    []
    "The transactions to be sent in the response."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceControlMessage anyProto))
    []
    "TODO TraceDoc"
  ]

docLocalTxSubmissionServer :: Documented (TraceLocalTxSubmissionServerEvent blk)
docLocalTxSubmissionServer = Documented [
    DocMsg
    (TraceReceivedTx anyProto)
    []
    "A transaction was received."
  ]

docMempool :: forall blk. Documented (TraceEventMempool blk)
docMempool = Documented [
    DocMsg
      (TraceMempoolAddedTx anyProto anyProto anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      ]
      "New, valid transaction that was added to the Mempool."
  , DocMsg
      (TraceMempoolRejectedTx anyProto anyProto anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      ]
      "New, invalid transaction thas was rejected and thus not added to\
      \ the Mempool."
  , DocMsg
      (TraceMempoolRemoveTxs [anyProto] anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      ]
      "Previously valid transactions that are no longer valid because of\
      \ changes in the ledger state. These transactions have been removed\
      \ from the Mempool."
  , DocMsg
      (TraceMempoolManuallyRemovedTxs [anyProto] [anyProto] anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      , ("cardano.node.txsProcessedNum", "TODO TraceDoc")
      ]
      "Transactions that have been manually removed from the Mempool."
  ]


docForge :: Documented (Either (TraceLabelCreds (TraceForgeEvent blk))
                               (TraceLabelCreds TraceStartLeadershipCheckPlus))
docForge = Documented [
    DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceStartLeadershipCheck anyProto)))
      [("cardano.node.aboutToLeadSlotLast", "TODO TraceDoc")]
      "Start of the leadership check."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceSlotIsImmutable anyProto anyProto anyProto)))
      [("cardano.node.slotIsImmutable", "TODO TraceDoc")]
      "Leadership check failed: the tip of the ImmutableDB inhabits the\
      \  current slot\
      \ \
      \  This might happen in two cases.\
      \ \
      \   1. the clock moved backwards, on restart we ignored everything from the\
      \      VolatileDB since it's all in the future, and now the tip of the\
      \      ImmutableDB points to a block produced in the same slot we're trying\
      \      to produce a block in\
      \ \
      \   2. k = 0 and we already adopted a block from another leader of the same\
      \      slot.\
      \ \
      \  We record both the current slot number as well as the tip of the\
      \  ImmutableDB.\
      \ \
      \ See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceBlockFromFuture anyProto anyProto)))
      [("cardano.node.blockFromFuture", "TODO TraceDoc")]
      "Leadership check failed: the current chain contains a block from a slot\
      \  /after/ the current slot\
      \ \
      \  This can only happen if the system is under heavy load.\
      \ \
      \  We record both the current slot number as well as the slot number of the\
      \  block at the tip of the chain.\
      \ \
      \  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceBlockContext anyProto anyProto anyProto)))
      [("cardano.node.blockContext", "TODO TraceDoc")]
      "We found out to which block we are going to connect the block we are about\
      \  to forge.\
      \ \
      \  We record the current slot number, the block number of the block to\
      \  connect to and its point.\
      \ \
      \  Note that block number of the block we will try to forge is one more than\
      \  the recorded block number."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNoLedgerState anyProto anyProto)))
      [("cardano.node.couldNotForgeSlotLast", "TODO TraceDoc")]
      "Leadership check failed: we were unable to get the ledger state for the\
      \  point of the block we want to connect to\
      \ \
      \  This can happen if after choosing which block to connect to the node\
      \  switched to a different fork. We expect this to happen only rather\
      \  rarely, so this certainly merits a warning; if it happens a lot, that\
      \  merits an investigation.\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceLedgerState anyProto anyProto)))
      [("cardano.node.ledgerState", "TODO TraceDoc")]
      "We obtained a ledger state for the point of the block we want to\
      \  connect to\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNoLedgerView anyProto anyProto)))
      [("cardano.node.couldNotForgeSlotLast", "TODO TraceDoc")]
      "Leadership check failed: we were unable to get the ledger view for the\
      \  current slot number\
      \ \
      \  This will only happen if there are many missing blocks between the tip of\
      \  our chain and the current slot.\
      \ \
      \  We record also the failure returned by 'forecastFor'."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceLedgerView anyProto)))
      [("cardano.node.ledgerView", "TODO TraceDoc")]
      "We obtained a ledger view for the current slot number\
      \ \
      \  We record the current slot number."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceForgeStateUpdateError anyProto anyProto)))
      [ ("cardano.node.operationalCertificateStartKESPeriod", "TODO TraceDoc")
      , ("cardano.node.operationalCertificateExpiryKESPeriod", "TODO TraceDoc")
      , ("cardano.node.currentKESPeriod", "TODO TraceDoc")
      , ("cardano.node.remainingKESPeriods", "TODO TraceDoc")
      ]
      "Updating the forge state failed.\
      \ \
      \  For example, the KES key could not be evolved anymore.\
      \ \
      \  We record the error returned by 'updateForgeState'."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNodeCannotForge anyProto anyProto)))
      [("cardano.node.nodeCannotForge", "TODO TraceDoc")]
      "We did the leadership check and concluded that we should lead and forge\
      \  a block, but cannot.\
      \ \
      \  This should only happen rarely and should be logged with warning severity.\
      \ \
      \  Records why we cannot forge a block."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNodeNotLeader anyProto)))
      [("cardano.node.nodeNotLeader", "TODO TraceDoc")]
      "We did the leadership check and concluded we are not the leader\
      \ \
      \  We record the current slot number"
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNodeIsLeader anyProto)))
      [("cardano.node.nodeIsLeader", "TODO TraceDoc")]
      "We did the leadership check and concluded we /are/ the leader\
      \\n\
      \  The node will soon forge; it is about to read its transactions from the\
      \  Mempool. This will be followed by ForgedBlock."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceForgedBlock anyProto anyProto anyProto anyProto)))
      [("cardano.node.forgedSlotLast", "TODO TraceDoc")]
      "We forged a block.\
      \\n\
      \  We record the current slot number, the point of the predecessor, the block\
      \  itself, and the total size of the mempool snapshot at the time we produced\
      \  the block (which may be significantly larger than the block, due to\
      \  maximum block size)\
      \\n\
      \  This will be followed by one of three messages:\
      \\n\
      \  * AdoptedBlock (normally)\
      \\n\
      \  * DidntAdoptBlock (rarely)\
      \\n\
      \  * ForgedInvalidBlock (hopefully never -- this would indicate a bug)"
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceDidntAdoptBlock anyProto anyProto)))
      [("cardano.node.notAdoptedSlotLast", "TODO TraceDoc")]
      "We did not adopt the block we produced, but the block was valid. We\
      \  must have adopted a block that another leader of the same slot produced\
      \  before we got the chance of adopting our own block. This is very rare,\
      \  this warrants a warning."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceForgedInvalidBlock anyProto anyProto anyProto)))
      [("cardano.node.forgedInvalidSlotLast", "TODO TraceDoc")]
      "We forged a block that is invalid according to the ledger in the\
      \  ChainDB. This means there is an inconsistency between the mempool\
      \  validation and the ledger validation. This is a serious error!"
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceAdoptedBlock anyProto anyProto [anyProto])))
      [("cardano.node.adoptedSlotLast", "TODO TraceDoc")]
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."
  , DocMsg
      (Right (TraceLabelCreds anyProto
        (TraceStartLeadershipCheckPlus anyProto 0 0 0.0)))
      [ ("cardano.node.aboutToLeadSlotLast", "TODO TraceDoc")
      , ("cardano.node.utxoSize", "TODO TraceDoc")
      , ("cardano.node.delegMapSize", "TODO TraceDoc")
      ]
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."

  ]

docForgeStateInfo :: Documented (TraceLabelCreds HotKey.KESInfo)
docForgeStateInfo = Documented [
    DocMsg
      (TraceLabelCreds anyProto (HotKey.KESInfo (KESPeriod 0) (KESPeriod 1) 2))
      []
      "kesStartPeriod \
      \\nkesEndPeriod is kesStartPeriod + tpraosMaxKESEvo\
      \\nkesEvolution is the current evolution or /relative period/."
    ]

docBlockchainTime :: Documented (TraceBlockchainTimeEvent t)
docBlockchainTime = Documented [
    DocMsg
      (TraceStartTimeInTheFuture anyProto anyProto)
      []
      "The start time of the blockchain time is in the future\
      \\n\
      \ We have to block (for 'NominalDiffTime') until that time comes."
  , DocMsg
      (TraceCurrentSlotUnknown anyProto anyProto)
      []
      "Current slot is not yet known\
      \\n\
      \ This happens when the tip of our current chain is so far in the past that\
      \ we cannot translate the current wallclock to a slot number, typically\
      \ during syncing. Until the current slot number is known, we cannot\
      \ produce blocks. Seeing this message during syncing therefore is\
      \ normal and to be expected.\
      \\n\
      \ We record the current time (the time we tried to translate to a 'SlotNo')\
      \ as well as the 'PastHorizonException', which provides detail on the\
      \ bounds between which we /can/ do conversions. The distance between the\
      \ current time and the upper bound should rapidly decrease with consecutive\
      \ 'CurrentSlotUnknown' messages during syncing."
  , DocMsg
      (TraceSystemClockMovedBack anyProto anyProto)
      []
      "The system clock moved back an acceptable time span, e.g., because of\
      \ an NTP sync.\
      \\n\
      \ The system clock moved back such that the new current slot would be\
      \ smaller than the previous one. If this is within the configured limit, we\
      \ trace this warning but *do not change the current slot*. The current slot\
      \ never decreases, but the current slot may stay the same longer than\
      \ expected.\
      \\n\
      \ When the system clock moved back more than the configured limit, we shut\
      \ down with a fatal exception."
  ]

docKeepAliveClient :: Documented (TraceKeepAliveClient peer)
docKeepAliveClient = Documented [
    DocMsg
      (AddSample anyProto anyProto anyProto)
      []
      "TODO TraceDoc"
  ]
