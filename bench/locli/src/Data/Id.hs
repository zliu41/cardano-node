{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module Data.Id
  ( Id (..)
  , ToId (..)
  , renderIdShortText
  , renderIdText
  , coerceId
  )
where

import Prelude (String, Show (..), error, id)
import Cardano.Prelude

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as AE
import Data.ByteString.Base16 as B16
import Data.ByteString.Short (ShortByteString, unpack, toShort)
import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Short (ShortText, fromString)
import Text.Builder qualified as T


newtype Id a =
  Id { idBytes :: ShortByteString }

instance Show (Id a) where
  show = T.unpack . ("\"" <>) . (<> "\"")  . renderIdText

instance ToJSON (Id a) where
  toJSON = AE.String . renderIdText

instance FromJSON (Id a) where
  parseJSON = AE.withText "Id"
    (\txt ->
       pure . Id . toShort
       . either (error $ "Invalid base16-encoded Id: " <> T.unpack txt) id
      . B16.decode $ T.encodeUtf8 txt)

class ToId a where
  toId :: a -> Id a

renderIdShortText :: Id a -> ShortText
renderIdShortText = fromString . concat . fmap hexString . unpack . idBytes

renderIdText :: Id a -> T.Text
renderIdText = T.run . mconcat . fmap hexWord8 . unpack . idBytes
  where
    hexWord8 :: Word8 -> T.Builder
    hexWord8 w = T.unsignedHexadecimal h <> T.unsignedHexadecimal l
     where
       (h,l :: Word8) = fromIntegral w `divMod` 16

coerceId :: Id a -> Id b
coerceId = coerce

{-# INLINE hexBytes #-}
hexBytes :: Word8 -> (Char, Char)
hexBytes w = (hex h, hex l)
 where
   (h,l) = fromIntegral w `divMod` 16
   hex :: Int -> Char
   hex = \case
     0  -> '0'
     1  -> '1'
     2  -> '2'
     3  -> '3'
     4  -> '4'
     5  -> '5'
     6  -> '6'
     7  -> '7'
     8  -> '8'
     9  -> '9'
     10 -> 'a'
     11 -> 'b'
     12 -> 'c'
     13 -> 'd'
     14 -> 'e'
     15 -> 'f'
     _  -> ' '

-- | Dump one byte into a 2 hexadecimal characters.
hexString :: Word8 -> String
hexString i = [h,l] where (h,l) = hexBytes i
