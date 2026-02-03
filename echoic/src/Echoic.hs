module Echoic (runEchoic) where

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Concurrent.STM (newTVarIO)
import Echoic.App (echoicApp)
import Echoic.Config (Config (..), VoiceLines (..))
import Echoic.OptParse (Settings (..), getSettings)
import Echoic.Speech (speakVoiceLineSync)
import Echoic.State (initialState, stateVoiceSpeed)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

runEchoic :: Config -> IO ()
runEchoic config = do
  Settings {..} <- getSettings

  speakVoiceLineSync settingVoicePath (stateVoiceSpeed initialState) (voiceStartup (configVoiceLines config))

  eventChan <- newBChan 10
  speechVar <- newTVarIO Nothing

  let app = echoicApp config settingVoicePath speechVar eventChan
      buildVty = VCP.mkVty V.defaultConfig

  vty <- buildVty
  _finalState <- customMain vty buildVty (Just eventChan) app initialState
  pure ()
