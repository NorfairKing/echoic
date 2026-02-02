{-# LANGUAGE ApplicativeDo #-}

module Echoic.OptParse where

import OptEnvConf
import Path
import Paths_echoic (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "echoic"

data Settings = Settings
  { settingVoicePath :: !(Path Abs File)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = withoutConfig $ do
  settingVoicePath <-
    filePathSetting
      [ help "Path to piper voice model (.onnx file)",
        name "voice",
        env "PIPER_VOICE"
      ]
  pure Settings {..}
