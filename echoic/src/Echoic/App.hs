module Echoic.App
  ( echoicApp,
    AppEvent (..),
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Data.Text as Text
import Echoic.State
import qualified Graphics.Vty as Vty

data AppEvent
  = SpeakComplete
  deriving (Show, Eq)

echoicApp :: App AppState AppEvent ResourceName
echoicApp =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const theAttrMap
    }

drawUI :: AppState -> [Widget ResourceName]
drawUI s =
  [ center $
      border $
        padAll 1 $
          vBox
            [ str $ "Mode: " <> show (stateMode s),
              str " ",
              str $ "Buffer: " <> Text.unpack (stateInputBuffer s),
              str " ",
              str "Press 'i' for insert mode, Esc for normal mode, 'q' to quit"
            ]
  ]

handleEvent :: BrickEvent ResourceName AppEvent -> EventM ResourceName AppState ()
handleEvent (VtyEvent (Vty.EvKey key mods)) = do
  s <- get
  case stateMode s of
    NormalMode -> handleNormalMode key mods
    InsertMode -> handleInsertMode key mods
handleEvent _ = pure ()

handleNormalMode :: Vty.Key -> [Vty.Modifier] -> EventM ResourceName AppState ()
handleNormalMode key _mods = case key of
  Vty.KChar 'q' -> halt
  Vty.KChar 'i' -> modify $ \s -> s {stateMode = InsertMode}
  _ -> pure ()

handleInsertMode :: Vty.Key -> [Vty.Modifier] -> EventM ResourceName AppState ()
handleInsertMode key _mods = case key of
  Vty.KEsc -> modify $ \s -> s {stateMode = NormalMode}
  Vty.KChar c -> modify $ \s -> s {stateInputBuffer = stateInputBuffer s <> Text.singleton c}
  Vty.KBS -> modify $ \s -> s {stateInputBuffer = Text.dropEnd 1 (stateInputBuffer s)}
  Vty.KEnter -> modify $ \s ->
    s
      { stateHistory = stateInputBuffer s : stateHistory s,
        stateInputBuffer = Text.empty
      }
  _ -> pure ()

theAttrMap :: AttrMap
theAttrMap = attrMap Vty.defAttr []
