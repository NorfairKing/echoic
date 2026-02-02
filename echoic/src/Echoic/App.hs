module Echoic.App
  ( echoicApp,
    AppEvent (..),
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Cursor.Brick.Text (selectedTextCursorWidget)
import Cursor.Text (TextCursor)
import qualified Cursor.Text as TextCursor
import Cursor.Types (DeleteOrUpdate (..))
import Data.Maybe (fromMaybe)
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
              str "Buffer: " <+> selectedTextCursorWidget MainViewport (stateInputBuffer s),
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
  Vty.KChar c -> modifyBufferM $ TextCursor.textCursorInsert c
  Vty.KBS -> modifyBufferDOU TextCursor.textCursorRemove
  Vty.KDel -> modifyBufferDOU TextCursor.textCursorDelete
  Vty.KLeft -> modifyBufferM TextCursor.textCursorSelectPrev
  Vty.KRight -> modifyBufferM TextCursor.textCursorSelectNext
  Vty.KEnter -> modify $ \s ->
    s
      { stateHistory = TextCursor.rebuildTextCursor (stateInputBuffer s) : stateHistory s,
        stateInputBuffer = TextCursor.emptyTextCursor
      }
  _ -> pure ()

modifyBufferM :: (TextCursor -> Maybe TextCursor) -> EventM ResourceName AppState ()
modifyBufferM f = modify $ \s -> s {stateInputBuffer = fromMaybe (stateInputBuffer s) (f (stateInputBuffer s))}

modifyBufferDOU :: (TextCursor -> Maybe (DeleteOrUpdate TextCursor)) -> EventM ResourceName AppState ()
modifyBufferDOU f = modify $ \s ->
  s
    { stateInputBuffer = case f (stateInputBuffer s) of
        Nothing -> stateInputBuffer s
        Just Deleted -> TextCursor.emptyTextCursor
        Just (Updated tc) -> tc
    }

theAttrMap :: AttrMap
theAttrMap = attrMap Vty.defAttr []
