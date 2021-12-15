{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-orphans #-}

module Cardano.Unlog.LogObject (module Cardano.Unlog.LogObject) where

import Prelude (String, error, head, id, show)
import Cardano.Prelude hiding (Text, head, show)

import Control.Monad (fail)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as LText
import Data.Text.Short qualified as Text
import Data.Text.Short (ShortText, fromText, toText)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Map qualified as Map
import Data.Vector (Vector)
import Quiet (Quiet (..))

import Ouroboros.Network.Block (BlockNo(..), SlotNo(..))

import Cardano.Logging.Resources.Types

import Data.Accum (zeroUTCTime)

type FastText = ShortText


readLogObjectStream :: JsonLogfile -> IO [LogObject]
readLogObjectStream (JsonLogfile f) =
  LBS.readFile f
    <&>
    fmap (either (LogObject zeroUTCTime "DecodeError" "" (TId "0") . LODecodeError)
                 id
          . AE.eitherDecode)
    . LBS.split (fromIntegral $ fromEnum '\n')

newtype JsonRunMetafile
  = JsonRunMetafile { unJsonRunMetafile :: FilePath }
  deriving (Show, Eq)

newtype JsonGenesisFile
  = JsonGenesisFile { unJsonGenesisFile :: FilePath }
  deriving (Show, Eq)

newtype JsonLogfile
  = JsonLogfile { unJsonLogfile :: FilePath }
  deriving (Show, Eq)

newtype JsonOutputFile
  = JsonOutputFile { unJsonOutputFile :: FilePath }
  deriving (Show, Eq)

newtype TextOutputFile
  = TextOutputFile { unTextOutputFile :: FilePath }
  deriving (Show, Eq)

newtype CsvOutputFile
  = CsvOutputFile { unCsvOutputFile :: FilePath }
  deriving (Show, Eq)

newtype OutputFile
  = OutputFile { unOutputFile :: FilePath }
  deriving (Show, Eq)

data LogObject
  = LogObject
    { loAt   :: !UTCTime
    , loKind :: !FastText
    , loHost :: !Host
    , loTid  :: !TId
    , loBody :: !LOBody
    }
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LogObject

instance ToJSON FastText where
  toJSON = String . toText

instance FromJSON FastText where
  parseJSON = AE.withText "String" $ pure . fromText

instance Print FastText where
  hPutStr   h = hPutStr   h . toText
  hPutStrLn h = hPutStrLn h . toText

newtype TId = TId { unTId :: FastText }
  deriving (Eq, Generic, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass NFData
  deriving Show via Quiet TId

newtype Hash = Hash { unHash :: FastText }
  deriving (Eq, Generic, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass NFData

shortHash :: Hash -> LText.Text
shortHash = toText . Text.take 6 . unHash

instance Show Hash where show = LText.unpack . toText . unHash

instance AE.ToJSONKey Hash where
  toJSONKey = AE.toJSONKeyText (toText . unHash)

newtype Host = Host { unHost :: FastText }
  deriving (Eq, Generic, Ord)
  deriving newtype (IsString, FromJSON, ToJSON)
  deriving anyclass NFData
  deriving Show via Quiet Host

instance FromJSON BlockNo where
  parseJSON o = BlockNo <$> parseJSON o
instance ToJSON BlockNo where
  toJSON (BlockNo x) = toJSON x

deriving instance NFData a => NFData (Resources a)

--
-- LogObject stream interpretation
--

interpreters :: Map FastText (Object -> Parser LOBody)
interpreters = Map.fromList
  [ (,) "TraceStartLeadershipCheck" $
    \v -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> (v .:? "utxoSize"     <&> fromMaybe 0)
            <*> (v .:? "chainDensity" <&> fromMaybe 0)

  , (,) "TraceBlockContext" $
    \v -> LOBlockContext
            <$> v .: "tipBlockNo"

  , (,) "TraceNodeIsLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure True

  , (,) "TraceNodeNotLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure False

  , (,) "TraceMempoolAddedTx" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRemoveTxs" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRejectedTx" $
    \_ -> pure LOMempoolRejectedTx

  , (,) "TraceLedgerEvent.TookSnapshot" $
    \_ -> pure LOLedgerTookSnapshot

  , (,) "TraceBenchTxSubSummary" $
    \v -> do
       x :: Object <- v .: "summary"
       LOGeneratorSummary
         <$> ((x .: "ssFailures" :: Parser [FastText])
              <&> null)
         <*> x .: "ssTxSent"
         <*> x .: "ssElapsed"
         <*> x .: "ssThreadwiseTps"

  , (,) "TraceBenchTxSubServAck" $
    \v -> LOTxsAcked <$> v .: "txIds"

  , (,) "Resources" $
    \v -> LOResources <$> parsePartialResourceStates (Object v)

  , (,) "TraceTxSubmissionCollected" $
    \v -> LOTxsCollected
            <$> v .: "count"

  , (,) "TraceTxSubmissionProcessed" $
    \v -> LOTxsProcessed
            <$> v .: "accepted"
            <*> v .: "rejected"

  , (,) "TraceForgedBlock" $
    \v -> LOBlockForged
            <$> v .: "block"
            <*> v .: "blockPrev"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,) "TraceAddBlockEvent.AddedToCurrentChain" $
    \v -> LOBlockAddedToCurrentChain
            <$> ((v .: "newtip")     <&> hashFromPoint)
            <*> pure Nothing
            <*> (v .:? "chainLengthDelta"
                -- Compat for node versions 1.27 and older:
                 <&> fromMaybe 1)
  -- TODO: we should clarify the distinction between the two cases (^ and v).
  , (,) "TraceAdoptedBlock" $
    \v -> LOBlockAddedToCurrentChain
            <$> v .: "blockHash"
            <*> ((v .: "blockSize") <&> Just)
            <*> pure 1
  , (,) "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock" $
    \v -> LOChainSyncServerSendHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,) "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock" $
    \v -> LOChainSyncServerSendHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  -- v, but not ^ -- how is that possible?
  , (,) "TraceBlockFetchServerSendBlock" $
    \v -> LOBlockFetchServerSending
            <$> v .: "block"
  , (,) "SendFetchRequest" $
    \v -> LOBlockFetchClientRequested
            <$> v .: "head"
            <*> v .: "length"
  , (,) "ChainSyncClientEvent.TraceDownloadedHeader" $
    \v -> LOChainSyncClientSeenHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,) "CompletedBlockFetch" $
    \v -> LOBlockFetchClientCompletedFetch
            <$> v .: "block"
  ]
 where
   hashFromPoint :: LText.Text -> Hash
   hashFromPoint = Hash . fromText . Prelude.head . LText.splitOn "@"

logObjectStreamInterpreterKeys :: [FastText]
logObjectStreamInterpreterKeys = Map.keys interpreters

data LOBody
  = LODecodeError !String

  -- general
  | LOResources !ResourceStats
  | LOLedgerTookSnapshot

  -- mempool & tx submission
  | LOMempoolTxs !Word64
  | LOMempoolRejectedTx
  | LOTxsAcked !(Vector FastText)
  | LOTxsCollected !Word64
  | LOTxsProcessed
    { loAccepted         :: !Word64
    , loRejected         :: !Word64
    }

  -- forging
  | LOTraceStartLeadershipCheck
    { loSlot             :: !SlotNo
    , loUTxO             :: !Word64
    , loDensity          :: !Float }
  | LOTraceLeadershipDecided
    { loSlot             :: !SlotNo
    , loLeader           :: !Bool }
  | LOBlockContext
    { loBlockNo          :: !BlockNo }
  | LOBlockForged
    { loBlock            :: !Hash
    , loPrev             :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlot             :: !SlotNo
    }

  -- chaindb
  | LOBlockAddedToCurrentChain
    { loBlock            :: !Hash
    , loSize             :: !(Maybe Int)
    , loLength           :: !Int
    }

  -- chainsync & blockfetch
  | LOChainSyncServerSendHeader
    { loBlock            :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlot             :: !SlotNo
    }
  | LOBlockFetchServerSending
    { loBlock            :: !Hash
    }
  | LOBlockFetchClientRequested
    { loBlock            :: !Hash
    , loLength           :: !Int
    }
  | LOChainSyncClientSeenHeader
    { loBlock            :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlot             :: !SlotNo
    }
  | LOBlockFetchClientCompletedFetch
    { loBlock            :: !Hash
    }

  | LOGeneratorSummary
    { loNoFailures       :: !Bool
    , loTxsSent          :: !Word64
    , loElapsed          :: !NominalDiffTime
    , loThreadwiseTps    :: !(Vector Float)
    }
  | LOAny !Object
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LOBody

instance FromJSON LogObject where
  parseJSON = AE.withObject "LogObject" $ \v -> do
    body :: Object <- v .: "data"
    -- XXX:  fix node causing the need for this workaround
    (,) unwrapped kind <- unwrap "credentials" "val" body
    LogObject
      <$> v .: "at"
      <*> pure kind
      <*> v .: "host"
      <*> v .: "thread"
      <*> case Map.lookup kind interpreters of
            Just interp -> interp unwrapped
            Nothing -> pure $ LOAny unwrapped
   where
     unwrap :: FastText -> FastText -> Object -> Parser (Object, FastText)
     unwrap wrappedKeyPred unwrapKey v = do
       kind <- (fromText <$>) <$> v .:? "kind"
       wrapped   :: Maybe FastText <-
         (fromText <$>) <$> v .:? toText wrappedKeyPred
       unwrapped :: Maybe Object <- v .:? toText unwrapKey
       case (kind, wrapped, unwrapped) of
         (Nothing, Just _, Just x) -> (,) <$> pure x <*> (fromText <$> x .: "kind")
         (Just kind0, _, _) -> pure (v, kind0)
         _ -> fail $ "Unexpected LogObject .data: " <> show v

extendObject :: FastText -> Value -> Value -> Value
extendObject k v (Object hm) = Object $ hm <> HM.singleton (toText k) v
extendObject k _ _ = error . Text.unpack $ "Summary key '" <> k <> "' does not serialise to an Object."

parsePartialResourceStates :: Value -> Parser (Resources Word64)
parsePartialResourceStates =
  AE.withObject "NodeSetup" $
    \o ->
      Resources
      <$> o .: "CentiCpu"
      <*> o .: "CentiGC"
      <*> o .: "CentiMut"
      <*> o .: "GcsMajor"
      <*> o .: "GcsMinor"
      <*> o .: "Alloc"
      <*> o .: "Live"
      <*> (o .:? "Heap" <&> fromMaybe 0)
      <*> o .: "RSS"
      <*> o .: "CentiBlkIO"
      <*> o .: "Threads"
