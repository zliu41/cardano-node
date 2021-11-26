{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.TraceDispatcher.Tracers.BasicInfo
  (
    BasicInfo(..)
  , BasicInfoCommon (..)
  , BasicInfoShelleyBased (..)
  , BasicInfoByron (..)
  , BasicInfoNetwork (..)
  , getBasicInfo
  , severityBasicInfo
  , namesForBasicInfo
  , docBasicInfo
  ) where


import           Cardano.Api.Protocol.Types (BlockType (..), protocolInfo)
import qualified Cardano.Chain.Genesis as Gen
import           Cardano.Node.Configuration.POM (NodeConfiguration, ncProtocol)
import           Cardano.Node.Protocol (SomeConsensusProtocol (..))
import           Cardano.Node.Types (protocolName)
import           Cardano.Slotting.Slot (EpochSize (..))
import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)
import           Data.Time (getCurrentTime)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Data.Version (showVersion)
import           Network.Socket (SockAddr)
import           Paths_cardano_node (version)

import           Cardano.Api (NetworkMagic (..))
import           Cardano.Config.Git.Rev (gitRev)
import           Cardano.Logging
import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..))
import           Cardano.Prelude hiding (trace)

import           Ouroboros.Network.NodeToNode (DiffusionMode (..))
import           Ouroboros.Network.Subscription.Dns (DnsSubscriptionTarget (..))
import           Ouroboros.Network.Subscription.Ip (IPSubscriptionTarget (..))


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

data BasicInfo =
    BICommon BasicInfoCommon
  | BIShelley BasicInfoShelleyBased
  | BIByron BasicInfoByron
  | BINetwork BasicInfoNetwork

data BasicInfoCommon = BasicInfoCommon {
    biConfigPath    :: FilePath
  , biNetworkMagic  :: NetworkMagic
  , biProtocol      :: Text
  , biVersion       :: Text
  , biCommit        :: Text
  , biNodeStartTime :: UTCTime
  }

data BasicInfoShelleyBased = BasicInfoShelleyBased {
    bisEra               :: Text
  , bisSystemStartTime   :: UTCTime
  , bisSlotLength        :: NominalDiffTime
  , bisEpochLength       :: Word64
  , bisSlotsPerKESPeriod :: Word64
}

data BasicInfoByron = BasicInfoByron {
    bibSystemStartTime :: UTCTime
  , bibSlotLength      :: NominalDiffTime
  , bibEpochLength     :: Word64
}

data BasicInfoNetwork = BasicInfoNetwork {
    niAddresses     :: [SocketOrSocketInfo SockAddr SockAddr]
  , niDiffusionMode :: DiffusionMode
  , niDnsProducers  :: [DnsSubscriptionTarget]
  , niIpProducers   :: IPSubscriptionTarget
  }

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

namesForBasicInfo :: BasicInfo -> [Text]
namesForBasicInfo BICommon {}  = ["Common"]
namesForBasicInfo BIShelley {} = ["ShelleyBased"]
namesForBasicInfo BIByron {}   = ["Byron"]
namesForBasicInfo BINetwork {} = ["Network"]

severityBasicInfo :: BasicInfo -> SeverityS
severityBasicInfo _ = Notice

instance LogFormatting BasicInfo where
  forHuman (BINetwork (BasicInfoNetwork {..})) =
      "Addresses " <> show niAddresses
      <> ", DiffusionMode " <> show niDiffusionMode
      <> ", DnsProducers " <> show niDnsProducers
      <> ", IpProducers " <> show niIpProducers
  forHuman (BIByron (BasicInfoByron {..})) =
      "Era Byron"
      <> ", Slot length " <> show bibSlotLength
      <> ", Epoch length " <> show bibEpochLength
  forHuman (BIShelley (BasicInfoShelleyBased {..})) =
      "Era " <> bisEra
      <> ", Slot length " <> show bisSlotLength
      <> ", Epoch length " <> show bisEpochLength
      <> ", Slots per KESPeriod " <> show bisSlotsPerKESPeriod
  forHuman (BICommon (BasicInfoCommon {..})) =
      "Config path " <> pack biConfigPath
      <> ", Network magic " <> show biNetworkMagic
      <> ", Protocol " <> show biProtocol
      <> ", Version " <> show biVersion
      <> ", Commit " <> show biCommit
      <> ", Node start time " <> show biNodeStartTime

  forMachine _dtal (BINetwork (BasicInfoNetwork {..})) =
      mkObject [ "kind" .= String "BasicInfoNetwork"
               , "addresses" .= String (show niAddresses)
               , "diffusionMode"  .= String (show niDiffusionMode)
               , "dnsProducers" .= String (show niDnsProducers)
               , "ipProducers" .= String (show niIpProducers)
               ]
  forMachine _dtal (BIByron (BasicInfoByron {..})) =
      mkObject [ "kind" .= String "BasicInfoByron"
               , "systemStartTime" .= String (show bibSystemStartTime)
               , "slotLength"  .= String (show bibSlotLength)
               , "epochLength" .= String (show bibEpochLength)
               ]
  forMachine _dtal (BIShelley (BasicInfoShelleyBased {..})) =
      mkObject [ "kind" .= String "BasicInfoShelleyBased"
               , "era"  .= String bisEra
               , "systemStartTime" .= String (show bisSystemStartTime)
               , "slotLength"  .= String (show bisSlotLength)
               , "epochLength" .= String (show bisEpochLength)
               , "slotsPerKESPeriod" .= String (show bisSlotsPerKESPeriod)
               ]
  forMachine _dtal (BICommon (BasicInfoCommon {..})) =
      mkObject [ "kind" .= String "BasicInfoCommon"
               , "configPath" .= String (pack biConfigPath)
               , "networkMagic"  .= String (show biNetworkMagic)
               , "protocol" .= String biProtocol
               , "version" .= String biVersion
               , "commit" .= String biCommit
               , "nodeStartTime" .= biNodeStartTime
               ]


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
