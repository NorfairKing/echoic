module Echoic (runEchoic) where

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Concurrent.STM (newTVarIO)
import Echoic.App (initialState)
import Echoic.App.Core (echoicApp)
import Echoic.Config (Config (..))
import Echoic.Env (EchoicEnv (..))
import Echoic.OptParse (Settings (..), getSettings)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

runEchoic :: Config -> IO ()
runEchoic config = do
  Settings {..} <- getSettings

  eventChan <- newBChan 10
  speechVar <- newTVarIO Nothing

  let env =
        EchoicEnv
          { envVoicePath = settingVoicePath,
            envSpeechVar = speechVar,
            envVoiceLines = configVoiceLines config
          }
      app = echoicApp config env eventChan
      buildVty = VCP.mkVty V.defaultConfig

  vty <- buildVty
  _finalState <- customMain vty buildVty (Just eventChan) app initialState
  pure ()
