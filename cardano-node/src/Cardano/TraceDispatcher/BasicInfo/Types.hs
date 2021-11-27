{-# LANGUAGE RecordWildCards #-}
module Cardano.TraceDispatcher.BasicInfo.Types
  (
    BasicInfo(..)
  , BasicInfoCommon (..)
  , BasicInfoShelleyBased (..)
  , BasicInfoByron (..)
  , BasicInfoNetwork (..)
  ) where


import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Network.Socket (SockAddr)

import           Cardano.Api (NetworkMagic (..))
import           Cardano.Logging
import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..))
import           Cardano.Prelude hiding (trace)

import           Ouroboros.Network.NodeToNode (DiffusionMode (..))
import           Ouroboros.Network.Subscription.Dns (DnsSubscriptionTarget (..))
import           Ouroboros.Network.Subscription.Ip (IPSubscriptionTarget (..))


instance LogFormatting (StartupTrace blk) where
  forHuman   = ppStartupInfoTrace
  forMachine = toObject
