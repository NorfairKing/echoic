module Echoic (runEchoic) where

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Concurrent.STM (newTVarIO)
import Echoic.App (echoicApp)
import Echoic.OptParse (Settings (..), getSettings)
import Echoic.Speech (speakSync)
import Echoic.State (initialState)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

runEchoic :: IO ()
runEchoic = do
  Settings {..} <- getSettings

  speakSync settingVoicePath "Echoic ready. Input mode."

  eventChan <- newBChan 10
  speechVar <- newTVarIO Nothing

  let app = echoicApp settingVoicePath speechVar eventChan
      buildVty = VCP.mkVty V.defaultConfig

  vty <- buildVty
  _finalState <- customMain vty buildVty (Just eventChan) app initialState
  pure ()
