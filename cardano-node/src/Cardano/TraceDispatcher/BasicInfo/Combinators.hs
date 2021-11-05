{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.BasicInfo.Combinators
  (
    getBasicInfo
  , severityBasicInfo
  , namesForBasicInfo
  , docBasicInfo
  ) where

import           Cardano.Config.Git.Rev (gitRev)
import           Cardano.Logging
import           Cardano.Prelude hiding (trace)
import           Paths_cardano_node (version)

import           Data.Text (pack)
import           Data.Time (getCurrentTime)
import           Data.Version (showVersion)

import           Cardano.Api.Protocol.Types (BlockType (..), protocolInfo)
import qualified Cardano.Chain.Genesis as Gen
import           Cardano.Node.Configuration.POM (NodeConfiguration, ncProtocol)
import           Cardano.Node.Protocol (SomeConsensusProtocol (..))
import           Cardano.Node.Types (protocolName)
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.TraceDispatcher.BasicInfo.Types

import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WCT
import           Ouroboros.Consensus.Byron.Ledger.Conversions
                     (fromByronEpochSlots, fromByronSlotLength,
                     genesisSlotLength)
import           Ouroboros.Consensus.Cardano.Block (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ByronPartialLedgerConfig (..),
                     ShelleyPartialLedgerConfig (..))
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode
                     (ConfigSupportsNode (..))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
                     (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
                     (shelleyLedgerGenesis)

import           Cardano.Ledger.Shelley.API as SL

severityBasicInfo :: BasicInfo -> SeverityS
severityBasicInfo _ = Notice

namesForBasicInfo :: BasicInfo -> [Text]
namesForBasicInfo BICommon {}  = ["Common"]
namesForBasicInfo BIShelley {} = ["ShelleyBased"]
namesForBasicInfo BIByron {}   = ["Byron"]
namesForBasicInfo BINetwork {} = ["Network"]


docBasicInfo :: Documented BasicInfo
docBasicInfo = Documented [
    DocMsg
      (BICommon anyProto)
      []
      "_biConfigPath_: is the path to the config in use. \
      \\n_biProtocol_: is the name of the protocol, e.g. \"Byron\", \"Shelley\" \
      \or \"Byron; Shelley\". \
      \\n_biVersion_: is the version of the node software running. \
      \\n_biCommit_: is the commit revision of the software running. \
      \\n_biNodeStartTime_: gives the time this node was started."
  , DocMsg
      (BIShelley anyProto)
      []
      "bisEra is the current era, e.g. \"Shelley\", \"Allegra\", \"Mary\" \
      \or \"Alonzo\". \
      \\n_bisSystemStartTime_: TODO JNF \
      \\n_bisSlotLength_: gives the length of a slot as time interval. \
      \\n_bisEpochLength_: gives the number of slots which forms an epoch. \
      \\n_bisSlotsPerKESPeriod_: gives the slots per KES period."
  , DocMsg
      (BIByron anyProto)
      []
      "_bibSystemStartTime_: TODO JNF \
      \\n_bibSlotLength_: gives the length of a slot as time interval. \
      \\n_bibEpochLength_: gives the number of slots which forms an epoch."
  , DocMsg
      (BINetwork anyProto)
      []
      "_niAddresses_: IPv4 or IPv6 socket ready to accept connections\
      \or diffusion addresses. \
      \\n_niDiffusionMode_: shows if the node runs only initiator or both\
      \initiator or responder node. \
      \\n_niDnsProducers_: shows the list of domain names to subscribe to. \
      \\n_niIpProducers_: shows the list of ip subscription addresses."
  ]

getBasicInfo ::
     NodeConfiguration
  -> SomeConsensusProtocol
  -> FilePath
  -> IO [BasicInfo]
getBasicInfo nc (SomeConsensusProtocol whichP pForInfo) fp = do
  nodeStartTime <- getCurrentTime
  let cfg = pInfoConfig $ protocolInfo pForInfo
      basicInfoCommon = BICommon $ BasicInfoCommon {
                biProtocol = pack . protocolName $ ncProtocol nc
              , biVersion  = pack . showVersion $ version
              , biCommit   = gitRev
              , biNodeStartTime = nodeStartTime
              , biConfigPath = fp
              , biNetworkMagic = getNetworkMagic $ Consensus.configBlock cfg
              }
      protocolDependentItems =
        case whichP of
          ByronBlockType ->
            let DegenLedgerConfig cfgByron = Consensus.configLedger cfg
            in [getGenesisValuesByron cfg cfgByron]
          ShelleyBlockType ->
            let DegenLedgerConfig cfgShelley = Consensus.configLedger cfg
            in [getGenesisValues "Shelley" cfgShelley]
          CardanoBlockType ->
            let CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo = Consensus.configLedger cfg
            in getGenesisValuesByron cfg cfgByron
               : getGenesisValues "Shelley" cfgShelley
               : getGenesisValues "Allegra" cfgAllegra
               : getGenesisValues "Mary"    cfgMary
               : [getGenesisValues "Alonzo"  cfgAlonzo]
  pure (basicInfoCommon : protocolDependentItems)
    where
      getGenesisValues era config =
        let genesis = shelleyLedgerGenesis $ shelleyLedgerConfig config
        in BIShelley $ BasicInfoShelleyBased {
            bisEra               = era
          , bisSystemStartTime   = SL.sgSystemStart genesis
          , bisSlotLength        = WCT.getSlotLength . WCT.mkSlotLength
                                      $ SL.sgSlotLength genesis
          , bisEpochLength       = unEpochSize . SL.sgEpochLength $ genesis
          , bisSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
        }
      getGenesisValuesByron cfg config =
        let genesis = byronLedgerConfig config
        in BIByron $ BasicInfoByron {
            bibSystemStartTime = WCT.getSystemStart . getSystemStart
                                  $ Consensus.configBlock cfg
          , bibSlotLength      = WCT.getSlotLength . fromByronSlotLength
                                  $ genesisSlotLength genesis
          , bibEpochLength     = unEpochSize . fromByronEpochSlots
                                  $ Gen.configEpochSlots genesis
          }
