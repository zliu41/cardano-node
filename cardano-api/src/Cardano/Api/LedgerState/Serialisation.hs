{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Codec.Serialise as CBOR
import           Data.SOP.Strict (K (..))
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.State as HFC
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope (Telescope (TS, TZ))
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import qualified Ouroboros.Consensus.Shelley.Eras as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley


encodeLedgerState :: LedgerState -> CBOR.Encoding
encodeLedgerState (LedgerState (HFC.HardForkLedgerState (HFC.HardForkState telescope))) =
  encodeTelescope telescope
  where
    encodeTelescope ::
      Telescope
        (K HFC.Past)
        (HFC.Current Ledger.LedgerState)
        (Consensus.CardanoEras Consensus.StandardCrypto) ->
      CBOR.Encoding
    encodeTelescope = \case
      TZ byron ->
        mconcat
          [CBOR.encodeListLen 1, encodeByronLedgerState byron]
      TS byron (TZ shelley) ->
        mconcat
          [CBOR.encodeListLen 2, encodePast byron, encodeShelleyLedgerState shelley]
      TS byron (TS shelley (TZ allegra)) ->
        mconcat
          [ CBOR.encodeListLen 3,
            encodePast byron,
            encodePast shelley,
            encodeShelleyLedgerState allegra
          ]
      TS byron (TS shelley (TS allegra (TZ mary))) ->
        mconcat
          [ CBOR.encodeListLen 4,
            encodePast byron,
            encodePast shelley,
            encodePast allegra,
            encodeShelleyLedgerState mary
          ]
      TS byron (TS shelley (TS allegra (TS mary (TZ alonzo)))) ->
        mconcat
          [ CBOR.encodeListLen 5,
            encodePast byron,
            encodePast shelley,
            encodePast allegra,
            encodePast mary,
            encodeShelleyLedgerState alonzo
          ]
      TS byron (TS shelley (TS allegra (TS mary (TS alonzo (TZ babbage))))) ->
        mconcat
          [ CBOR.encodeListLen 6,
            encodePast byron,
            encodePast shelley,
            encodePast allegra,
            encodePast mary,
            encodePast alonzo,
            encodeShelleyLedgerState babbage
          ]

    encodePast :: forall era. K HFC.Past era -> CBOR.Encoding
    encodePast (K (HFC.Past start end)) =
      mconcat
        [ CBOR.encodeListLen 2,
          CBOR.encode start,
          CBOR.encode end
        ]

    encodeByronLedgerState :: HFC.Current Ledger.LedgerState Byron.ByronBlock -> CBOR.Encoding
    encodeByronLedgerState (HFC.Current start state) =
      mconcat
        [ CBOR.encodeListLen 2,
          CBOR.encode start,
          Byron.encodeByronLedgerState state
        ]

    encodeShelleyLedgerState ::
      (Shelley.ShelleyCompatible proto era) =>
      HFC.Current Shelley.LedgerState (Shelley.ShelleyBlock proto era) ->
      CBOR.Encoding
    encodeShelleyLedgerState (HFC.Current start state) =
      mconcat
        [ CBOR.encodeListLen 2,
          CBOR.encode start,
          Shelley.encodeShelleyLedgerState state
        ]

decodeLedgerState :: forall s. CBOR.Decoder s LedgerState
decodeLedgerState = LedgerState . HFC.HardForkLedgerState . HFC.HardForkState <$> decodeTelescope
  where
    decodeTelescope ::
      forall s'.
      CBOR.Decoder
        s'
        ( Telescope
            (K HFC.Past)
            (HFC.Current Shelley.LedgerState)
            (Consensus.CardanoEras Shelley.StandardCrypto)
        )
    decodeTelescope = do
      CBOR.decodeListLen >>= \case
        1 -> do
          byron <- decodeByronLedgerState
          pure $ TZ byron
        2 -> do
          byron <- decodePast
          shelley <- decodeShelleyLedgerState
          pure $ TS byron (TZ shelley)
        3 -> do
          byron <- decodePast
          shelley <- decodePast
          allegra <- decodeShelleyLedgerState
          pure $ TS byron (TS shelley (TZ allegra))
        4 -> do
          byron <- decodePast
          shelley <- decodePast
          allegra <- decodePast
          mary <- decodeShelleyLedgerState
          pure $ TS byron (TS shelley (TS allegra (TZ mary)))
        5 -> do
          byron <- decodePast
          shelley <- decodePast
          allegra <- decodePast
          mary <- decodePast
          alonzo <- decodeShelleyLedgerState
          pure $ TS byron (TS shelley (TS allegra (TS mary (TZ alonzo))))
        6 -> do
          byron <- decodePast
          shelley <- decodePast
          allegra <- decodePast
          mary <- decodePast
          alonzo <- decodePast
          babbage <- decodeShelleyLedgerState
          pure $ TS byron (TS shelley (TS allegra (TS mary (TS alonzo (TZ babbage)))))
        other -> fail $ "decodeLedgerState: unexpected length: " <> show other

    decodePast :: forall era s'. CBOR.Decoder s' (K HFC.Past era)
    decodePast = do
      CBOR.decodeListLenOf 2
      K <$> (HFC.Past <$> CBOR.decode <*> CBOR.decode)

    decodeByronLedgerState ::
      forall s'. CBOR.Decoder s' (HFC.Current Ledger.LedgerState Byron.ByronBlock)
    decodeByronLedgerState = do
      CBOR.decodeListLenOf 2
      HFC.Current <$> CBOR.decode <*> Byron.decodeByronLedgerState

    decodeShelleyLedgerState ::
      forall proto era s'.
      (Shelley.ShelleyCompatible proto era) =>
      CBOR.Decoder
        s'
        (HFC.Current Shelley.LedgerState (Shelley.ShelleyBlock proto era))
    decodeShelleyLedgerState = do
      CBOR.decodeListLenOf 2
      HFC.Current <$> CBOR.decode <*> Shelley.decodeShelleyLedgerState
