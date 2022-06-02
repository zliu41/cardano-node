{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Notifications
  ( restoreEmailSettings
  , saveEmailSettings
  ) where

import           Control.Exception.Extra (ignore)
import           Control.Monad (unless)
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher (..), cipherInit, ctrCombine, nullIV)
import           Crypto.Error (CryptoError, eitherCryptoError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Aeson (decodeStrict', encode)
import           Data.Text (pack, unpack)
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils

restoreEmailSettings :: Window -> UI ()
restoreEmailSettings window = readSavedEmailSettings >>= setEmailSettings
 where
  setEmailSettings settings = do
    setValue (unpack $ esSMTPHost settings)  "es-smtp-host"
    setValue (show   $ esSMTPPort settings)  "es-smtp-port"
    setValue (unpack $ esUsername settings)  "es-username"
    setValue (unpack $ esPassword settings)  "es-password"
    setValue (show   $ esSSL settings)       "es-ssl"
    setValue (unpack $ esEmailFrom settings) "es-email-from"
    setValue (unpack $ esEmailTo settings)   "es-email-to"
    setValue (unpack $ esSubject settings)   "es-subject"

  setValue elValue elId =
    unless (null elValue || elValue == "-1") $
      findAndSet (set value elValue) window elId
  
saveEmailSettings :: Window -> UI ()
saveEmailSettings window = do
  smtpHost  <- findAndGetValue window "es-smtp-host"
  smtpPort  <- findAndGetValue window "es-smtp-port"
  username  <- findAndGetValue window "es-username"
  password  <- findAndGetValue window "es-password"
  ssl       <- findAndGetValue window "es-ssl"
  emailFrom <- findAndGetValue window "es-email-from"
  emailTo   <- findAndGetValue window "es-email-to"
  subject   <- findAndGetValue window "es-subject"
  let settings = EmailSettings
        { esSMTPHost  = pack smtpHost
        , esSMTPPort  = readInt (pack smtpPort) (-1)
        , esUsername  = pack username
        , esPassword  = pack password
        , esSSL       = read ssl
        , esEmailFrom = pack emailFrom
        , esEmailTo   = pack emailTo
        , esSubject   = pack subject
        }
  liftIO . ignore $ do
    (pathToEmailSettings, _) <- getPathsToNotificationsSettings
    -- Encrypt JSON-content to avoid saving user's private data in "plain mode".
    case encryptJSON . LBS.toStrict . encode $ settings of
      Right encryptedJSON -> BS.writeFile pathToEmailSettings encryptedJSON
      Left _ -> return ()

readSavedEmailSettings :: UI EmailSettings
readSavedEmailSettings = liftIO $ do
  (pathToEmailSettings, _) <- getPathsToNotificationsSettings
  encryptedSettings <- BS.readFile pathToEmailSettings
  case decryptJSON encryptedSettings of
    Left _ -> return defaultSettings
    Right jsonSettings ->
      case decodeStrict' jsonSettings of
        Nothing -> return defaultSettings
        Just (settings :: EmailSettings) -> return settings
 where
  defaultSettings = EmailSettings
    { esSMTPHost  = ""
    , esSMTPPort  = -1
    , esUsername  = ""
    , esPassword  = ""
    , esSSL       = TLS
    , esEmailFrom = ""
    , esEmailTo   = ""
    , esSubject   = ""
    }

encryptJSON :: BS.ByteString -> Either CryptoError BS.ByteString
encryptJSON plainJSON = ctrCombine
  <$> cInit
  <*> pure nullIV
  <*> pure plainJSON
 where
  cInit :: Either CryptoError AES256
  cInit = eitherCryptoError $ cipherInit key

  key :: BS.ByteString
  key = "TRALIVALI"

decryptJSON :: BS.ByteString -> Either CryptoError BS.ByteString
decryptJSON = encryptJSON -- Encryption/decryption is symmetric.
