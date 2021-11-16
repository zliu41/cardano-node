{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.ChainDB.Docu
  ( docChainDBTraceEvent
  ) where

import           Cardano.Logging

import           Cardano.TraceDispatcher.Era.Byron ()
import           Cardano.TraceDispatcher.Era.Shelley ()

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB


docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk)
docChainDBTraceEvent = Documented [
    DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.IgnoreBlockOlderThanK anyProto))
      []
      "A block with a 'BlockNo' more than @k@ back than the current tip\
      \ was ignored."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.IgnoreBlockAlreadyInVolatileDB anyProto))
      []
      "A block that is already in the Volatile DB was ignored."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.IgnoreInvalidBlock anyProto anyProto))
      []
      "A block that is already in the Volatile DB was ignored."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddedBlockToQueue
          anyProto anyProto))
      []
      "The block was added to the queue and will be added to the ChainDB by\
      \ the background thread. The size of the queue is included.."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.BlockInTheFuture anyProto anyProto))
      []
      "The block is from the future, i.e., its slot number is greater than\
      \ the current slot (the second argument)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddedBlockToVolatileDB anyProto anyProto anyProto))
      []
      "A block was added to the Volatile DB"
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.TryAddToCurrentChain anyProto))
      []
      "The block fits onto the current chain, we'll try to use it to extend\
      \ our chain."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.TrySwitchToAFork anyProto anyProto))
      []
      "The block fits onto some fork, we'll try to switch to that fork (if\
      \ it is preferable to our chain)"
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.StoreButDontChange anyProto))
      []
      "The block fits onto some fork, we'll try to switch to that fork (if\
      \ it is preferable to our chain)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddedToCurrentChain [] anyProto anyProto anyProto))
      [("cardano.node.density",
        "The actual number of blocks created over the maximum expected number\
        \ of blocks that could be created over the span of the last @k@ blocks.")
      , ("cardano.node.slots",
        "Number of slots in this chain fragment.")
      , ("cardano.node.blocks",
        "Number of blocks in this chain fragment.")
      , ("cardano.node.slotInEpoch",
        "Relative slot number of the tip of the current chain within the\
        \epoch..")
      , ("cardano.node.epoch",
        "In which epoch is the tip of the current chain.")
      ]
      "The new block fits onto the current chain (first\
      \ fragment) and we have successfully used it to extend our (new) current\
      \ chain (second fragment)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.SwitchedToAFork [] anyProto anyProto anyProto))
      [ ("cardano.node.density",
        "The actual number of blocks created over the maximum expected number\
        \ of blocks that could be created over the span of the last @k@ blocks.")
      , ("cardano.node.slots",
        "Number of slots in this chain fragment.")
      , ("cardano.node.blocks",
        "Number of blocks in this chain fragment.")
      , ("cardano.node.slotInEpoch",
        "Relative slot number of the tip of the current chain within the\
        \epoch..")
      , ("cardano.node.epoch",
        "In which epoch is the tip of the current chain.")
      ]
      "The new block fits onto some fork and we have switched to that fork\
      \ (second fragment), as it is preferable to our (previous) current chain\
      \ (first fragment)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.InvalidBlock anyProto anyProto)))
      []
      "An event traced during validating performed while adding a block.\
      \ A point was found to be invalid."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.InvalidCandidate anyProto)))
      []
      "An event traced during validating performed while adding a block.\
      \ A candidate chain was invalid."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.ValidCandidate anyProto)))
      []
      "An event traced during validating performed while adding a block.\
      \ A candidate chain was valid."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.CandidateContainsFutureBlocks anyProto anyProto)))
      []
      "An event traced during validating performed while adding a block.\
      \ Candidate contains headers from the future which do no exceed the\
      \ clock skew."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew anyProto anyProto)))
      []
      "An event traced during validating performed while adding a block.\
      \ Candidate contains headers from the future which exceed the\
      \ clock skew."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.ChainSelectionForFutureBlock anyProto))
      []
      "Run chain selection for a block that was previously from the future.\
      \ This is done for all blocks from the future each time a new block is\
      \ added."
  , DocMsg
      (ChainDB.TraceFollowerEvent ChainDB.NewFollower)
      []
      "A new follower was created."
  , DocMsg
      (ChainDB.TraceFollowerEvent
        (ChainDB.FollowerNoLongerInMem anyProto))
      []
      "The follower was in the 'FollowerInImmutableDB' state and is switched to\
      \ the 'FollowerInMem' state."
  , DocMsg
      (ChainDB.TraceFollowerEvent
        (ChainDB.FollowerSwitchToMem anyProto anyProto))
      []
      "The follower was in the 'FollowerInImmutableDB' state and is switched to\
      \ the 'FollowerInMem' state."
  , DocMsg
      (ChainDB.TraceFollowerEvent
        (ChainDB.FollowerNewImmIterator anyProto anyProto))
      []
      "The follower is in the 'FollowerInImmutableDB' state but the iterator is\
      \ exhausted while the ImmDB has grown, so we open a new iterator to\
      \ stream these blocks too."
  , DocMsg
      (ChainDB.TraceCopyToImmutableDBEvent
        (ChainDB.CopiedBlockToImmutableDB anyProto))
      []
      "A block was successfully copied to the ImmDB."
  , DocMsg
      (ChainDB.TraceCopyToImmutableDBEvent
        ChainDB.NoBlocksToCopyToImmutableDB)
      []
      "There are no block to copy to the ImmDB."
  , DocMsg
      (ChainDB.TraceGCEvent
        (ChainDB.ScheduledGC anyProto anyProto))
      []
      "There are no block to copy to the ImmDB."
  , DocMsg
      (ChainDB.TraceGCEvent
        (ChainDB.PerformedGC anyProto))
      []
      "There are no block to copy to the ImmDB."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.InvalidBlock anyProto anyProto)))
      []
      "A point was found to be invalid."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.InvalidCandidate anyProto)))
      []
      "A candidate chain was invalid."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.ValidCandidate anyProto)))
      []
      "A candidate chain was valid."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.CandidateContainsFutureBlocks anyProto anyProto)))
      []
      "Candidate contains headers from the future which do not exceed the\
      \ clock skew."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew anyProto anyProto)))
      []
      "Candidate contains headers from the future which exceed the\
      \ clock skew, making them invalid."

  , DocMsg
      (ChainDB.TraceOpenEvent
        (ChainDB.OpenedDB anyProto anyProto))
      []
      "The ChainDB was opened."
  , DocMsg
      (ChainDB.TraceOpenEvent
        (ChainDB.ClosedDB anyProto anyProto))
      []
      "The ChainDB was closed."
  , DocMsg
      (ChainDB.TraceOpenEvent
        (ChainDB.OpenedImmutableDB anyProto anyProto))
      []
      "The ImmDB was opened."
  , DocMsg
      (ChainDB.TraceOpenEvent
        ChainDB.OpenedVolatileDB)
      []
      "The VolatileDB was opened."
  , DocMsg
      (ChainDB.TraceOpenEvent
        ChainDB.OpenedLgrDB)
      []
      "The LedgerDB was opened."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.UnknownRangeRequested anyProto))
      []
      "An unknown range was requested, see 'UnknownRange'."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.StreamFromVolatileDB anyProto anyProto anyProto))
      []
      "Stream only from the VolatileDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.StreamFromImmutableDB anyProto anyProto))
      []
      "Stream only from the ImmDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.StreamFromBoth anyProto anyProto anyProto))
      []
      "Stream from both the VolatileDB and the ImmDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.BlockMissingFromVolatileDB anyProto))
      []
      "A block is no longer in the VolatileDB because it has been garbage\
      \ collected. It might now be in the ImmDB if it was part of the\
      \ current chain."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.BlockWasCopiedToImmutableDB anyProto))
      []
      "A block that has been garbage collected from the VolatileDB is now\
      \ found and streamed from the ImmDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.BlockGCedFromVolatileDB anyProto))
      []
      "A block is no longer in the VolatileDB and isn't in the ImmDB\
      \ either; it wasn't part of the current chain."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        ChainDB.SwitchBackToVolatileDB)
      []
      "We have streamed one or more blocks from the ImmDB that were part\
      \ of the VolatileDB when initialising the iterator. Now, we have to look\
      \ back in the VolatileDB again because the ImmDB doesn't have the\
      \ next block we're looking for."
  , DocMsg
      (ChainDB.TraceLedgerEvent
        (LedgerDB.InvalidSnapshot anyProto anyProto))
      []
      "An on disk snapshot was skipped because it was invalid."
  , DocMsg
      (ChainDB.TraceLedgerEvent
        (LedgerDB.TookSnapshot anyProto anyProto))
      []
      "A snapshot was written to disk."
  , DocMsg
      (ChainDB.TraceLedgerEvent
        (LedgerDB.DeletedSnapshot anyProto))
      []
      "An old or invalid on-disk snapshot was deleted."

  , DocMsg
      (ChainDB.TraceLedgerReplayEvent
        (LedgerDB.ReplayFromGenesis anyProto))
      []
      "There were no LedgerDB snapshots on disk, so we're replaying all\
      \ blocks starting from Genesis against the initial ledger.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
  , DocMsg
      (ChainDB.TraceLedgerReplayEvent
        (LedgerDB.ReplayFromSnapshot anyProto anyProto anyProto))
      []
      "There was a LedgerDB snapshot on disk corresponding to the given tip.\
      \ We're replaying more recent blocks against it.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
  , DocMsg
      (ChainDB.TraceLedgerReplayEvent
        (LedgerDB.ReplayedBlock anyProto anyProto anyProto))
      []
      "We replayed the given block (reference) on the genesis snapshot\
      \ during the initialisation of the LedgerDB.\
      \\n\
      \ The @blockInfo@ parameter corresponds replayed block and the @replayTo@\
      \ parameter corresponds to the block at the tip of the ImmDB, i.e.,\
      \ the last block to replay."

  , DocMsg
      (ChainDB.TraceImmutableDBEvent ImmDB.NoValidLastLocation)
      []
      "No valid last location was found"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.ValidatedLastLocation anyProto anyProto))
      []
      "The last location was validatet"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.ValidatingChunk anyProto))
      []
      "The chunk was validatet"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.MissingChunkFile anyProto))
      []
      "Chunk file is missing"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.InvalidChunkFile anyProto anyProto))
      []
      "Chunk file is invalid"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.ChunkFileDoesntFit anyProto anyProto))
      []
      "The hash of the last block in the previous epoch doesn't match the\
      \ previous hash of the first block in the current epoch"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.MissingPrimaryIndex anyProto))
      []
      "The primary index is missing."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.MissingSecondaryIndex anyProto))
      []
      "The secondary index is missing."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.InvalidPrimaryIndex anyProto))
      []
      "The primary index is invalid."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.InvalidSecondaryIndex anyProto))
      []
      "The secondary index is invalid."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.RewritePrimaryIndex anyProto))
      []
      "The primary index gets rewritten."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.RewriteSecondaryIndex anyProto))
      []
      "The secondary index gets rewritten."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.Migrating ""))
      []
      "Performing a migration of the on-disk files."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.DeletingAfter anyProto))
      []
      "Delete after"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent ImmDB.DBAlreadyClosed)
      []
      "The immutable DB is already closed"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent ImmDB.DBClosed)
      []
      "Closing the immutable DB"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TraceCurrentChunkHit anyProto anyProto)))
      []
      "Current chunk found in the cache."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunkHit anyProto anyProto)))
      []
      "Past chunk found in the cache"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunkMiss anyProto anyProto)))
      []
      "Past chunk was not found in the cache"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunkEvict anyProto anyProto)))
      []
      "The least recently used past chunk was evicted because the cache\
      \ was full."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunksExpired anyProto anyProto)))
      []
      "Past chunks were expired from the cache because they haven't been\
      \ used for a while."

  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        VolDB.DBAlreadyClosed)
      []
      "When closing the DB it was found itis closed already."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        VolDB.DBAlreadyOpen)
      []
      "TODO TracerDoc"
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.Truncate anyProto anyProto anyProto))
      []
      "Truncates a file up to offset because of the error."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.InvalidFileNames anyProto))
      []
      "Reports a list of invalid file paths."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.BlockAlreadyHere anyProto))
      []
      "A block was found to be already in the DB."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.TruncateCurrentFile anyProto))
      []
      "TODO TracerDoc"
  ]
