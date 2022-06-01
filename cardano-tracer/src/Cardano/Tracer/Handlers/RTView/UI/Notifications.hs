{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Notifications
  ( restoreEmailSettings
  , saveEmailSettings
  ) where

import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (unless)
import           Data.Aeson (decodeFileStrict', encodeFile)
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
    encodeFile pathToEmailSettings settings

readSavedEmailSettings :: UI EmailSettings
readSavedEmailSettings = liftIO $ do
  (emailSettings, _) <- getPathsToNotificationsSettings
  try_ (decodeFileStrict' emailSettings) >>= \case
    Right (Just (settings :: EmailSettings)) -> return settings
    _ -> return defaultSettings
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
