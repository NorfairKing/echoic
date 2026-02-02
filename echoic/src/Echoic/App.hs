module Echoic.App
  ( echoicApp,
    AppEvent (..),
  )
where

import Brick
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Cursor.Brick.Text (selectedTextCursorWidget)
import Cursor.Text (TextCursor)
import qualified Cursor.Text as TextCursor
import Cursor.Types (DeleteOrUpdate (..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Echoic.Shell (OutputResult (..), runShellCommand)
import Echoic.Speech (SpeechHandle, cancelSpeech, speakAsync)
import Echoic.State
import qualified Graphics.Vty as Vty
import Path (Abs, File, Path)

data AppEvent
  = CommandComplete OutputResult
  deriving (Show)

echoicApp ::
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  BChan AppEvent ->
  App AppState AppEvent ResourceName
echoicApp voicePath speechVar eventChan =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent voicePath speechVar eventChan,
      appStartEvent = pure (),
      appAttrMap = const theAttrMap
    }

drawUI :: AppState -> [Widget ResourceName]
drawUI s = case stateMode s of
  InputMode -> drawInputMode s
  OutputMode -> drawOutputMode s

drawInputMode :: AppState -> [Widget ResourceName]
drawInputMode s =
  [ vBox
      [ str "$ " <+> selectedTextCursorWidget MainViewport (stateInputBuffer s),
        str " ",
        str "[Ctrl+R: read | Enter: run | Esc: cancel speech]"
      ]
  ]

drawOutputMode :: AppState -> [Widget ResourceName]
drawOutputMode s =
  [ vBox $
      case stateOutput s of
        Nothing ->
          [str "No output", str " ", str "[i: input | q: quit]"]
        Just ob ->
          let total = length (outputLines ob)
              idx = outputIndex ob
              currentLine =
                if null (outputLines ob)
                  then "(empty output)"
                  else Text.unpack (outputLines ob !! idx)
           in [ str $ "$ " <> Text.unpack (outputCommand ob),
                str $ "Line " <> show (idx + 1) <> "/" <> show (max 1 total),
                str " ",
                str currentLine,
                str " ",
                str "[j/k: navigate | .: read | Esc: cancel | i: input | q: quit]"
              ]
  ]

handleEvent ::
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  BChan AppEvent ->
  BrickEvent ResourceName AppEvent ->
  EventM ResourceName AppState ()
handleEvent voicePath speechVar eventChan event = do
  s <- get
  case event of
    VtyEvent (Vty.EvKey key mods) ->
      case stateMode s of
        InputMode -> handleInputMode voicePath speechVar eventChan key mods
        OutputMode -> handleOutputMode voicePath speechVar key mods
    AppEvent (CommandComplete result) -> do
      let allLines = resultStdout result <> resultStderr result
          ob =
            OutputBuffer
              { outputLines = if null allLines then ["(no output)"] else allLines,
                outputIndex = 0,
                outputCommand = maybe "" (TextCursor.rebuildTextCursor . stateInputBuffer) (Just s)
              }
      modify $ \st ->
        st
          { stateMode = OutputMode,
            stateOutput = Just ob,
            stateHistory = outputCommand ob : stateHistory st,
            stateInputBuffer = TextCursor.emptyTextCursor
          }
      case outputLines ob of
        (firstLine : _) -> speakLine voicePath speechVar firstLine
        [] -> speakText voicePath speechVar "no output"
    _ -> pure ()

handleInputMode ::
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  BChan AppEvent ->
  Vty.Key ->
  [Vty.Modifier] ->
  EventM ResourceName AppState ()
handleInputMode voicePath speechVar eventChan key mods = case (key, mods) of
  (Vty.KChar 'r', [Vty.MCtrl]) -> do
    s <- get
    let input = Text.unpack $ TextCursor.rebuildTextCursor (stateInputBuffer s)
    speakText voicePath speechVar (if null input then "empty" else input)
  (Vty.KEsc, []) -> do
    liftIO $ cancelCurrentSpeech speechVar
  (Vty.KEnter, []) -> do
    s <- get
    let cmd = TextCursor.rebuildTextCursor (stateInputBuffer s)
    if Text.null cmd
      then speakText voicePath speechVar "empty command"
      else liftIO $ do
        _ <- async $ do
          result <- runShellCommand cmd
          writeBChan eventChan (CommandComplete result)
        pure ()
  (Vty.KChar c, []) -> modifyBufferM $ TextCursor.textCursorInsert c
  (Vty.KBS, []) -> modifyBufferDOU TextCursor.textCursorRemove
  (Vty.KDel, []) -> modifyBufferDOU TextCursor.textCursorDelete
  (Vty.KLeft, []) -> modifyBufferM TextCursor.textCursorSelectPrev
  (Vty.KRight, []) -> modifyBufferM TextCursor.textCursorSelectNext
  _ -> pure ()

handleOutputMode ::
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  Vty.Key ->
  [Vty.Modifier] ->
  EventM ResourceName AppState ()
handleOutputMode voicePath speechVar key mods = case (key, mods) of
  (Vty.KChar '.', []) -> do
    s <- get
    case stateOutput s of
      Nothing -> pure ()
      Just ob ->
        let idx = outputIndex ob
         in if null (outputLines ob)
              then speakText voicePath speechVar "no output"
              else speakLine voicePath speechVar (outputLines ob !! idx)
  (Vty.KChar 'j', []) -> do
    s <- get
    case stateOutput s of
      Nothing -> pure ()
      Just ob ->
        let total = length (outputLines ob)
            idx = outputIndex ob
         in if idx >= total - 1
              then speakText voicePath speechVar "bottom"
              else do
                let newIdx = idx + 1
                modify $ \st -> st {stateOutput = Just ob {outputIndex = newIdx}}
                speakLine voicePath speechVar (outputLines ob !! newIdx)
  (Vty.KChar 'k', []) -> do
    s <- get
    case stateOutput s of
      Nothing -> pure ()
      Just ob ->
        let idx = outputIndex ob
         in if idx <= 0
              then speakText voicePath speechVar "top"
              else do
                let newIdx = idx - 1
                modify $ \st -> st {stateOutput = Just ob {outputIndex = newIdx}}
                speakLine voicePath speechVar (outputLines ob !! newIdx)
  (Vty.KEsc, []) -> do
    liftIO $ cancelCurrentSpeech speechVar
  (Vty.KChar 'i', []) -> do
    modify $ \s -> s {stateMode = InputMode}
    speakText voicePath speechVar "input mode"
  (Vty.KChar 'q', []) -> halt
  _ -> pure ()

speakText :: Path Abs File -> TVar (Maybe SpeechHandle) -> String -> EventM ResourceName AppState ()
speakText voicePath speechVar text = liftIO $ do
  cancelCurrentSpeech speechVar
  handle <- speakAsync voicePath text
  atomically $ writeTVar speechVar (Just handle)

speakLine :: Path Abs File -> TVar (Maybe SpeechHandle) -> Text -> EventM ResourceName AppState ()
speakLine voicePath speechVar line = speakText voicePath speechVar (Text.unpack line)

cancelCurrentSpeech :: TVar (Maybe SpeechHandle) -> IO ()
cancelCurrentSpeech speechVar = do
  mHandle <- atomically $ do
    h <- readTVar speechVar
    writeTVar speechVar Nothing
    pure h
  for_ mHandle cancelSpeech

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
