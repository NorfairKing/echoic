module Echoic.App.Core
  ( echoicApp,
  )
where

import Brick
import Brick.BChan (BChan)
import Echoic.App
import Echoic.App.Event (handleEvent, runE)
import Echoic.App.Render (drawUI)
import Echoic.Config (Config)
import Echoic.Env (EchoicEnv)
import qualified Graphics.Vty as Vty

echoicApp ::
  Config ->
  EchoicEnv ->
  BChan AppEvent ->
  App AppState AppEvent ResourceName
echoicApp config env eventChan =
  App
    { appDraw = drawUI config,
      appChooseCursor = neverShowCursor,
      appHandleEvent = runE env . handleEvent config eventChan,
      appStartEvent = pure (),
      appAttrMap = const theAttrMap
    }

theAttrMap :: AttrMap
theAttrMap = attrMap Vty.defAttr []
