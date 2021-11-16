{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Network.Docu
  ( docTChainSync
  , docTTxSubmission
  , docTStateQuery
  , docTBlockFetch
  , docTTxSubmissionNode
  , docTTxSubmission2Node
  , docIPSubscription
  , docDNSSubscription
  , docDNSResolver
  , docErrorPolicy
  , docLocalErrorPolicy
  , docAcceptPolicy
  , docMux
  , docHandshake
  , docLocalHandshake
  , docDiffusionInit
  ) where

import           Cardano.Prelude
import           Control.Monad.Class.MonadTime
import           Data.Time.Clock (secondsToDiffTime)
import qualified Network.DNS as DNS
import           Network.Mux (MiniProtocolNum (..), MuxBearerState (..),
                     MuxTrace (..), WithMuxBearer (..))
import           Network.Mux.Types (MiniProtocolDir (..), MuxSDUHeader (..),
                     RemoteClockModel (..))
import qualified Network.Socket as Socket
import           Unsafe.Coerce
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Logging
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)

import           Ouroboros.Network.Block (Point, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..),
                     WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..),
                     Message (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Message (..))
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TXS
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TXS
import           Ouroboros.Network.Snocket (FileDescriptor,
                     LocalAddress (..), socketFileDescriptor)
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..),
                     WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (ConnectResult (..),
                     LocalAddresses (..), SubscriptionTrace (..))


protoPeer :: peer
protoPeer = unsafeCoerce (NtN.ConnectionId protoSockAddr protoSockAddr)

protoLocalAdresses :: LocalAddresses addr
protoLocalAdresses = LocalAddresses Nothing Nothing Nothing

protoDiffTime :: DiffTime
protoDiffTime = secondsToDiffTime 1

protoDomain :: DNS.Domain
protoDomain = "www.example.org"

protoLocalAdress :: LocalAddress
protoLocalAdress = LocalAddress "loopback"

protoMuxSDUHeader :: MuxSDUHeader
protoMuxSDUHeader = MuxSDUHeader {
      mhTimestamp = RemoteClockModel 1
    , mhNum       = MiniProtocolNum 1
    , mhDir       = InitiatorDir
    , mhLength    = 1
    }

protoMuxBearerState :: MuxBearerState
protoMuxBearerState = Mature

protoMiniProtocolNum :: MiniProtocolNum
protoMiniProtocolNum = MiniProtocolNum 1

protoMiniProtocolDir :: MiniProtocolDir
protoMiniProtocolDir = InitiatorDir

protoSomeException :: SomeException
protoSomeException = SomeException (AssertionFailed "just fooled")

protoSockAddr :: Socket.SockAddr
protoSockAddr = Socket.SockAddrUnix "loopback"

protoLocalAddress :: LocalAddress
protoLocalAddress = LocalAddress "loopback"

protoFilePath :: FilePath
protoFilePath = "loopback"

{-# NOINLINE protoFileDescriptor #-}
protoFileDescriptor :: FileDescriptor
protoFileDescriptor =
  unsafePerformIO $ do
    sock <- Socket.mkSocket 111
    socketFileDescriptor sock



------------------------------------

docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSync = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto MsgRequestNext)))
        []
        "Request the next update from the producer. The response can be a roll\
        \forward, a roll back or wait."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto MsgAwaitReply)))
        []
        "Acknowledge the request but require the consumer to wait for the next\
        \update. This means that the consumer is synced with the producer, and\
        \the producer is waiting for its own chain state to change."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto MsgAwaitReply)))
        []
        "Tell the consumer to extend their chain with the given header.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgRollForward anyProto anyProto))))
        []
        "Tell the consumer to extend their chain with the given header.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgRollBackward anyProto anyProto))))
        []
        "Tell the consumer to roll back to a given point on their chain.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgFindIntersect [anyProto]))))
        []
        "Ask the producer to try to find an improved intersection point between\
        \the consumer and producer's chains. The consumer sends a sequence of\
        \points and it is up to the producer to find the first intersection point\
        \on its chain and send it back to the consumer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgIntersectFound anyProto anyProto))))
        []
        "The reply to the consumer about an intersection found.\
        \The consumer can decide weather to send more points.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgIntersectNotFound anyProto))))
        []
        "The reply to the consumer that no intersection was found: none of the\
        \points the consumer supplied are on the producer chain.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            MsgDone)))
        []
        "We have to explain to the framework what our states mean, in terms of\
        \which party has agency in each state.\
        \\n\
        \Idle states are where it is for the client to send a message,\
        \busy states are where the server is expected to send a reply."
  ]

docTTxSubmission :: Documented
   (BlockFetch.TraceLabelPeer
      localPeer
      (TraceSendRecv
         (LTS.LocalTxSubmission
            (GenTx blk) (ApplyTxErr blk))))
docTTxSubmission = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto (LTS.MsgSubmitTx anyProto))))
        []
        "The client submits a single transaction and waits a reply."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto LTS.MsgAcceptTx)))
        []
        "The server can reply to inform the client that it has accepted the\
        \transaction."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto (LTS.MsgRejectTx anyProto))))
        []
        "The server can reply to inform the client that it has rejected the\
        \transaction. A reason for the rejection is included."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto LTS.MsgDone)))
        []
        "The client can terminate the protocol."
  ]

docTStateQuery :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (LSQ.LocalStateQuery blk (Point blk) query)))
docTStateQuery = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto (LSQ.MsgAcquire Nothing))))
        []
        "The client requests that the state as of a particular recent point on\
        \the server's chain (within K of the tip) be made available to query,\
        \and waits for confirmation or failure.\
        \\n\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto LSQ.MsgAcquired)))
        []
        "The server can confirm that it has the state at the requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgFailure anyProto))))
        []
        "The server can report that it cannot obtain the state for the\
        \requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgQuery anyProto))))
        []
        "The client can perform queries on the current acquired state."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgResult anyProto anyProto))))
        []
        "The server must reply with the queries."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              LSQ.MsgRelease)))
        []
        "The client can instruct the server to release the state. This lets\
        \the server free resources."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgReAcquire Nothing))))
        []
        "This is like 'MsgAcquire' but for when the client already has a\
        \state. By moveing to another state directly without a 'MsgRelease' it\
        \enables optimisations on the server side (e.g. moving to the state for\
        \the immediate next block).\
        \\n\
        \Note that failure to re-acquire is equivalent to 'MsgRelease',\
        \rather than keeping the exiting acquired state.\
        \\n\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              LSQ.MsgDone)))
        []
        "The client can terminate the protocol."
  ]

docTBlockFetch :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (BlockFetch x (Point blk))))
docTBlockFetch = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (MsgRequestRange anyProto))))
        []
        "Request range of blocks."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              MsgStartBatch)))
        []
        "Start block streaming."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              MsgNoBlocks)))
        []
        "Respond that there are no blocks."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (MsgBlock undefined))))
        []
        "Stream a single block."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              MsgBatchDone)))
        []
        "End of block streaming."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              MsgClientDone)))
        []
        "Client termination message."
  ]

docTTxSubmissionNode :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission (GenTxId blk) (GenTx blk))))
docTTxSubmissionNode = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (TXS.MsgRequestTxIds anyProto 1 1))))
        []
        "Request a non-empty list of transaction identifiers from the client,\
        \and confirm a number of outstanding transaction identifiers.\
        \\n\
        \With 'TokBlocking' this is a a blocking operation: the response will\
        \always have at least one transaction identifier, and it does not expect\
        \a prompt response: there is no timeout. This covers the case when there\
        \is nothing else to do but wait. For example this covers leaf nodes that\
        \rarely, if ever, create and submit a transaction.\
        \\n\
        \With 'TokNonBlocking' this is a non-blocking operation: the response\
        \may be an empty list and this does expect a prompt response. This\
        \covers high throughput use cases where we wish to pipeline, by\
        \interleaving requests for additional transaction identifiers with\
        \requests for transactions, which requires these requests not block.\
        \\n\
        \The request gives the maximum number of transaction identifiers that\
        \can be accepted in the response. This must be greater than zero in the\
        \'TokBlocking' case. In the 'TokNonBlocking' case either the numbers\
        \acknowledged or the number requested must be non-zero. In either case,\
        \the number requested must not put the total outstanding over the fixed\
        \protocol limit.\
        \\n\
        \The request also gives the number of outstanding transaction\
        \identifiers that can now be acknowledged. The actual transactions\
        \to acknowledge are known to the peer based on the FIFO order in which\
        \they were provided.\
        \\n\
        \There is no choice about when to use the blocking case versus the\
        \non-blocking case, it depends on whether there are any remaining\
        \unacknowledged transactions (after taking into account the ones\
        \acknowledged in this message):\
        \\n\
        \* The blocking case must be used when there are zero remaining\
        \  unacknowledged transactions.\
        \\n\
        \* The non-blocking case must be used when there are non-zero remaining\
        \  unacknowledged transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (TXS.MsgReplyTxIds anyProto))))
        []
        "Reply with a list of transaction identifiers for available\
        \transactions, along with the size of each transaction.\
        \\n\
        \The list must not be longer than the maximum number requested.\
        \\n\
        \In the 'StTxIds' 'StBlocking' state the list must be non-empty while\
        \in the 'StTxIds' 'StNonBlocking' state the list may be empty.\
        \\n\
        \These transactions are added to the notional FIFO of outstanding\
        \transaction identifiers for the protocol.\
        \\n\
        \The order in which these transaction identifiers are returned must be\
        \the order in which they are submitted to the mempool, to preserve\
        \dependent transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (TXS.MsgRequestTxs [anyProto]))))
        []
        "Request one or more transactions corresponding to the given \
        \transaction identifiers. \
        \\n\
        \While it is the responsibility of the replying peer to keep within \
        \pipelining in-flight limits, the sender must also cooperate by keeping \
        \the total requested across all in-flight requests within the limits. \
        \\n\
        \It is an error to ask for transaction identifiers that were not \
        \previously announced (via 'MsgReplyTxIds'). \
        \\n\
        \It is an error to ask for transaction identifiers that are not \
        \outstanding or that were already asked for."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (TXS.MsgReplyTxs [anyProto]))))
        []
        "Reply with the requested transactions, or implicitly discard.\
        \\n\
        \Transactions can become invalid between the time the transaction \
        \identifier was sent and the transaction being requested. Invalid \
        \(including committed) transactions do not need to be sent.\
        \\n\
        \Any transaction identifiers requested but not provided in this reply \
        \should be considered as if this peer had never announced them. (Note \
        \that this is no guarantee that the transaction is invalid, it may still \
        \be valid and available from another peer)."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              TXS.MsgDone)))
        []
        "Termination message, initiated by the client when the server is \
        \making a blocking call for more transaction identifiers."
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  ]

docTTxSubmission2Node :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))))
docTTxSubmission2Node = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              MsgHello)))
        []
        "Client side hello message."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (MsgTalk (TXS.MsgRequestTxIds anyProto 1 1)))))
        []
        "Request a non-empty list of transaction identifiers from the client, \
        \and confirm a number of outstanding transaction identifiers. \
        \\n\
        \With 'TokBlocking' this is a a blocking operation: the response will \
        \always have at least one transaction identifier, and it does not expect \
        \a prompt response: there is no timeout. This covers the case when there \
        \is nothing else to do but wait. For example this covers leaf nodes that \
        \rarely, if ever, create and submit a transaction. \
        \\n\
        \With 'TokNonBlocking' this is a non-blocking operation: the response \
        \may be an empty list and this does expect a prompt response. This \
        \covers high throughput use cases where we wish to pipeline, by \
        \interleaving requests for additional transaction identifiers with \
        \requests for transactions, which requires these requests not block. \
        \\n\
        \The request gives the maximum number of transaction identifiers that \
        \can be accepted in the response. This must be greater than zero in the \
        \'TokBlocking' case. In the 'TokNonBlocking' case either the numbers \
        \acknowledged or the number requested must be non-zero. In either case, \
        \the number requested must not put the total outstanding over the fixed \
        \protocol limit. \
        \\n\
        \The request also gives the number of outstanding transaction \
        \identifiers that can now be acknowledged. The actual transactions \
        \to acknowledge are known to the peer based on the FIFO order in which \
        \they were provided. \
        \\n\
        \There is no choice about when to use the blocking case versus the \
        \non-blocking case, it depends on whether there are any remaining \
        \unacknowledged transactions (after taking into account the ones \
        \acknowledged in this message): \
        \\n\
        \* The blocking case must be used when there are zero remaining \
        \  unacknowledged transactions. \
        \\n\
        \* The non-blocking case must be used when there are non-zero remaining \
        \  unacknowledged transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (MsgTalk (TXS.MsgReplyTxIds anyProto)))))
        []
        "Reply with a list of transaction identifiers for available\
        \transactions, along with the size of each transaction.\
        \\n\
        \The list must not be longer than the maximum number requested.\
        \\n\
        \In the 'StTxIds' 'StBlocking' state the list must be non-empty while\
        \in the 'StTxIds' 'StNonBlocking' state the list may be empty.\
        \\n\
        \These transactions are added to the notional FIFO of outstanding\
        \transaction identifiers for the protocol.\
        \\n\
        \The order in which these transaction identifiers are returned must be\
        \the order in which they are submitted to the mempool, to preserve\
        \dependent transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (MsgTalk (TXS.MsgRequestTxs [anyProto])))))
        []
        "Request one or more transactions corresponding to the given \
        \transaction identifiers. \
        \\n\
        \While it is the responsibility of the replying peer to keep within\
        \pipelining in-flight limits, the sender must also cooperate by keeping\
        \the total requested across all in-flight requests within the limits.\
        \\n\
        \It is an error to ask for transaction identifiers that were not\
        \previously announced (via 'MsgReplyTxIds').\
        \\n\
        \It is an error to ask for transaction identifiers that are not\
        \outstanding or that were already asked for."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (MsgTalk (TXS.MsgReplyTxs [anyProto])))))
        []
        "Reply with the requested transactions, or implicitly discard.\
        \\n\
        \Transactions can become invalid between the time the transaction\
        \identifier was sent and the transaction being requested. Invalid\
        \(including committed) transactions do not need to be sent.\
        \\n\
        \Any transaction identifiers requested but not provided in this reply\
        \should be considered as if this peer had never announced them. (Note\
        \that this is no guarantee that the transaction is invalid, it may still\
        \be valid and available from another peer)."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (MsgTalk TXS.MsgDone))))
        []
        "Termination message, initiated by the client when the server is\
        \making a blocking call for more transaction identifiers."
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  ]

docIPSubscription :: Documented (WithIPList (SubscriptionTrace Socket.SockAddr))
docIPSubscription = Documented $ map withIPList (undoc docSubscription)
  where
    withIPList (DocMsg v nl comment) =
      DocMsg (WithIPList protoLocalAdresses [] v) nl ("IP Subscription: " <> comment)

docDNSSubscription :: Documented (WithDomainName (SubscriptionTrace Socket.SockAddr))
docDNSSubscription = Documented $ map withDomainName (undoc docSubscription)
  where
    withDomainName (DocMsg v nl comment) =
      DocMsg (WithDomainName protoDomain v) nl ("DNS Subscription: " <> comment)

docSubscription :: Documented (SubscriptionTrace Socket.SockAddr)
docSubscription = Documented [
      DocMsg
        (SubscriptionTraceConnectStart anyProto)
        []
        "Connection Attempt Start with destination."
    , DocMsg
        (SubscriptionTraceConnectEnd anyProto ConnectSuccess)
        []
        "Connection Attempt end with destination and outcome."
    , DocMsg
        (SubscriptionTraceSocketAllocationException anyProto protoSomeException)
        []
        "Socket Allocation Exception with destination and the exception."
    , DocMsg
        (SubscriptionTraceConnectException anyProto protoSomeException)
        []
        "Connection Attempt Exception with destination and exception."
    , DocMsg
        (SubscriptionTraceTryConnectToPeer anyProto)
        []
        "Trying to connect to peer with address."
    , DocMsg
        (SubscriptionTraceSkippingPeer anyProto)
        []
        "Skipping peer with address."
    , DocMsg
        SubscriptionTraceSubscriptionRunning
        []
        "Required subscriptions started."
    , DocMsg
        (SubscriptionTraceSubscriptionWaiting 1)
        []
        "Waiting on address with active connections."
    , DocMsg
        SubscriptionTraceSubscriptionFailed
        []
        "Failed to start all required subscriptions."
    , DocMsg
        (SubscriptionTraceSubscriptionWaitingNewConnection anyProto)
        []
        "Waiting delay time before attempting a new connection."
    , DocMsg
        (SubscriptionTraceStart 1)
        []
        "Starting Subscription Worker with a valency."
    , DocMsg
        (SubscriptionTraceRestart anyProto 1 2)
        []
        "Restarting Subscription after duration with desired valency and\
        \ current valency."
    , DocMsg
        (SubscriptionTraceConnectionExist anyProto)
        []
        "Connection exists to destination."
    , DocMsg
        (SubscriptionTraceUnsupportedRemoteAddr anyProto)
        []
        "Unsupported remote target address."
    , DocMsg
        SubscriptionTraceMissingLocalAddress
        []
        "Missing local address."
    , DocMsg
        (SubscriptionTraceApplicationException anyProto protoSomeException)
        []
        "Application Exception occured."
    , DocMsg
        (SubscriptionTraceAllocateSocket anyProto)
        []
        "Allocate socket to address."
    , DocMsg
        (SubscriptionTraceCloseSocket anyProto)
        []
        "Closed socket to address."
  ]

-- WithDomainName has strict constructors

docDNSResolver :: Documented (WithDomainName DnsTrace)
docDNSResolver = Documented [
      DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupException anyProto))
        []
        "A DNS lookup exception occured."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAError anyProto))
        []
        "A lookup failed with an error."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAAAAError anyProto))
        []
        "AAAA lookup failed with an error."
    , DocMsg
        (WithDomainName protoDomain
          DnsTraceLookupIPv4First)
        []
        "Returning IPv4 address first."
    , DocMsg
        (WithDomainName protoDomain
          DnsTraceLookupIPv6First)
        []
        "Returning IPv6 address first."
    , DocMsg
        (WithDomainName protoDomain
          DnsTraceLookupIPv6First)
        []
        "Returning IPv6 address first."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAResult [anyProto]))
        []
        "Lookup A result."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAAAAResult [anyProto]))
        []
        "Lookup AAAA result."
    ]

docErrorPolicy :: Documented (WithAddr Socket.SockAddr ErrorPolicyTrace)
docErrorPolicy = docErrorPolicy' protoSockAddr

docLocalErrorPolicy :: Documented (WithAddr LocalAddress ErrorPolicyTrace)
docLocalErrorPolicy = docErrorPolicy' protoLocalAdress

-- WithAddr has strict constructors

docErrorPolicy' :: adr -> Documented (WithAddr adr ErrorPolicyTrace)
docErrorPolicy' adr = Documented [
      DocMsg
        (WithAddr adr
          (ErrorPolicySuspendPeer anyProto protoDiffTime protoDiffTime))
        []
        "Suspending peer with a given exception."
    , DocMsg
        (WithAddr adr
          (ErrorPolicySuspendConsumer anyProto protoDiffTime))
        []
        "Suspending consumer."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyLocalNodeError anyProto))
        []
        "caught a local exception."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumePeer)
        []
        "Resume a peer (both consumer and producer)."
    , DocMsg
        (WithAddr adr
          ErrorPolicyKeepSuspended)
        []
        "Consumer was suspended until producer will resume."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumeConsumer)
        []
        "Resume consumer."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumeProducer)
        []
        "Resume producer."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyUnhandledApplicationException anyProto))
        []
        "An application throwed an exception, which was not handled."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyUnhandledConnectionException anyProto))
        []
        "'connect' throwed an exception, which was not handled by any\
        \ 'ErrorPolicy'."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyAcceptException anyProto))
        []
        "'accept' throwed an exception."
    ]

docAcceptPolicy :: Documented NtN.AcceptConnectionsPolicyTrace
docAcceptPolicy = Documented [
      DocMsg
        (NtN.ServerTraceAcceptConnectionRateLimiting anyProto 2)
        []
        "Rate limiting accepting connections,\
        \ delaying next accept for given time, currently serving n connections."
      , DocMsg
        (NtN.ServerTraceAcceptConnectionHardLimit 2)
        []
        "Hard rate limit reached,\
        \ waiting until the number of connections drops below n."
  ]

-- WithMuxBearer has strict constructors
-- Eveything strict in MuxTrace

docMux :: Documented (WithMuxBearer peer MuxTrace)
docMux = Documented [
      DocMsg
        (WithMuxBearer protoPeer
          MuxTraceRecvHeaderStart)
        []
        "Bearer receive header start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvHeaderEnd protoMuxSDUHeader))
        []
        "Bearer receive header end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvDeltaQObservation protoMuxSDUHeader anyProto))
        []
        "Bearer DeltaQ observation."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvDeltaQSample 1.0 1 1 1.0 1.0 1.0 1.0 ""))
        []
        "Bearer DeltaQ sample."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvStart 1))
        []
        "Bearer receive start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvEnd 1))
        []
        "Bearer receive end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceSendStart protoMuxSDUHeader))
        []
        "Bearer send start."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceSendEnd)
        []
        "Bearer send end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceState protoMuxBearerState))
        []
        "State."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceCleanExit protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Miniprotocol terminated cleanly."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceExceptionExit
              protoMiniProtocolNum protoMiniProtocolDir protoSomeException))
        []
        "Miniprotocol terminated with exception."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelRecvStart protoMiniProtocolNum))
        []
        "Channel receive start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelRecvEnd protoMiniProtocolNum 1))
        []
        "Channel receive end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelSendStart protoMiniProtocolNum 1))
        []
        "Channel send start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelSendEnd protoMiniProtocolNum))
        []
        "Channel send end."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceHandshakeStart)
        []
        "Handshake start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceHandshakeClientEnd protoDiffTime))
        []
        "Handshake client end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceHandshakeClientError protoSomeException protoDiffTime))
        []
        "Handshake client error."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceHandshakeServerError protoSomeException))
        []
        "Handshake server error."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceSDUReadTimeoutException)
        []
        "Timed out reading SDU."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceSDUWriteTimeoutException)
        []
        "Timed out writing SDU."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceStartEagerly protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Eagerly started."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceStartOnDemand protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Preparing to start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceStartedOnDemand protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Started on demand."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceTerminating protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Terminating."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceShutdown)
        []
        "Mux shutdown."
  ]

docHandshake :: Documented NtN.HandshakeTr
docHandshake = Documented [
      DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgProposeVersions anyProto))))
        []
        "Propose versions together with version parameters.  It must be\
        \ encoded to a sorted list.."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgAcceptVersion anyProto anyProto))))
        []
        "The remote end decides which version to use and sends chosen version.\
        \The server is allowed to modify version parameters."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgRefuse anyProto))))
        []
        "It refuses to run any version."
    ]

docLocalHandshake :: Documented NtC.HandshakeTr
docLocalHandshake = Documented [
      DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgProposeVersions anyProto))))
        []
        "Propose versions together with version parameters.  It must be\
        \ encoded to a sorted list.."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgAcceptVersion anyProto anyProto))))
        []
        "The remote end decides which version to use and sends chosen version.\
        \The server is allowed to modify version parameters."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (HS.MsgRefuse anyProto))))
        []
        "It refuses to run any version."
    ]

-- Everything strict in DiffusionInitializationTracer
docDiffusionInit :: Documented ND.DiffusionInitializationTracer
docDiffusionInit = Documented [
    DocMsg
      (ND.RunServer protoSockAddr)
      []
      "RunServer TODO"
  , DocMsg
      (ND.RunLocalServer protoLocalAddress)
      []
      "RunLocalServer TODO"
  , DocMsg
      (ND.UsingSystemdSocket protoFilePath)
      []
      "UsingSystemdSocket TODO"
  , DocMsg
      (ND.CreateSystemdSocketForSnocketPath protoFilePath)
      []
      "CreateSystemdSocketForSnocketPath TODO"
  , DocMsg
      (ND.CreatedLocalSocket protoFilePath)
      []
      "CreatedLocalSocket TODO"
  , DocMsg
      (ND.ConfiguringLocalSocket protoFilePath protoFileDescriptor)
      []
      "ConfiguringLocalSocket TODO"
  , DocMsg
      (ND.ListeningLocalSocket protoFilePath protoFileDescriptor)
      []
      "ListeningLocalSocket TODO"
  , DocMsg
      (ND.LocalSocketUp protoFilePath protoFileDescriptor)
      []
      "LocalSocketUp TODO"
  , DocMsg
      (ND.CreatingServerSocket protoSockAddr)
      []
      "CreatingServerSocket TODO"
  , DocMsg
      (ND.ConfiguringServerSocket protoSockAddr)
      []
      "ConfiguringServerSocket TODO"
  , DocMsg
      (ND.ListeningServerSocket protoSockAddr)
      []
      "ListeningServerSocket TODO"
  , DocMsg
      (ND.ServerSocketUp protoSockAddr)
      []
      "ServerSocketUp TODO"
  , DocMsg
      (ND.UnsupportedLocalSystemdSocket protoSockAddr)
      []
      "UnsupportedLocalSystemdSocket TODO"
  , DocMsg
      ND.UnsupportedReadySocketCase
      []
      "UnsupportedReadySocketCase TODO"
  , DocMsg
      (ND.DiffusionErrored protoSomeException)
      []
      "DiffusionErrored TODO"
  ]
