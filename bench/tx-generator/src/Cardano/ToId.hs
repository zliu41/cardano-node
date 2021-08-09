{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.ToId () where

import Cardano.Prelude

import Data.ByteString.Short (toShort)

import Cardano.Api

import Data.Id


instance ToId (Tx era) where
  toId = Id . toShort . serialiseToRawBytes . getTxId . getTxBody
