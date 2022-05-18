module Cardano.Api.Alonzo.Render
  ( renderBadInputsUTxOErr
  , renderValueNotConservedErr
  , renderTxId
  ) where

import           Cardano.Ledger.Shelley.API hiding (ShelleyBasedEra)
import           Cardano.Prelude
import           Data.Aeson (Value(..))
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Prelude hiding ((.), map, show)

import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
  "This transaction consumed " <> show consumed <> " but produced " <> show produced

renderTxId :: Consensus.TxId (GenTx (ShelleyBlock protocol era)) -> Text
renderTxId = error "TODO implement"
