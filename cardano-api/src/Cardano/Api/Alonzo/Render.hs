module Cardano.Api.Alonzo.Render
  ( renderBadInputsUTxOErr
  , renderValueNotConservedErr
  ) where

import           Cardano.Ledger.Shelley.API hiding (ShelleyBasedEra)
import           Cardano.Prelude
import           Data.Aeson (Value(..))
import           Prelude hiding ((.), map, show)

import qualified Data.Set as Set

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
  "This transaction consumed " <> show consumed <> " but produced " <> show produced
