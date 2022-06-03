{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LedgerState.Serialisation
  ( encodeLedgerState,
    decodeLedgerState,
  )
where

import           Prelude

import           Cardano.Api.LedgerState (LedgerState (LedgerState))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.SOP.Strict (K (K), NP (Nil, (:*)), fn, type (:.:) (Comp))
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common as HFC
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley

encodeLedgerState :: LedgerState -> CBOR.Encoding
encodeLedgerState (LedgerState (HFC.HardForkLedgerState st)) =
  HFC.encodeTelescope
    (byron :* shelley :* allegra :* mary :* alonzo :* babbage :* Nil)
    st
  where
    byron = fn (K . Byron.encodeByronLedgerState)
    shelley = fn (K . Shelley.encodeShelleyLedgerState)
    allegra = fn (K . Shelley.encodeShelleyLedgerState)
    mary = fn (K . Shelley.encodeShelleyLedgerState)
    alonzo = fn (K . Shelley.encodeShelleyLedgerState)
    babbage = fn (K . Shelley.encodeShelleyLedgerState)

decodeLedgerState :: forall s. CBOR.Decoder s LedgerState
decodeLedgerState =
  LedgerState . HFC.HardForkLedgerState
    <$> HFC.decodeTelescope (byron :* shelley :* allegra :* mary :* alonzo :* babbage :* Nil)
  where
    byron = Comp Byron.decodeByronLedgerState
    shelley = Comp Shelley.decodeShelleyLedgerState
    allegra = Comp Shelley.decodeShelleyLedgerState
    mary = Comp Shelley.decodeShelleyLedgerState
    alonzo = Comp Shelley.decodeShelleyLedgerState
    babbage = Comp Shelley.decodeShelleyLedgerState
