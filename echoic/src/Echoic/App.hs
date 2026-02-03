module Echoic.App
  ( echoicApp,
    AppEvent (..),
  )
where

import Brick
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Cursor.Brick.Text (selectedTextCursorWidget)
import Cursor.Text (TextCursor)
import qualified Cursor.Text as TextCursor
import Cursor.Types (DeleteOrUpdate (..))
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Echoic.Config
import Echoic.Shell (OutputResult (..), runShellCommand)
import Echoic.Speech (SpeechHandle, cancelSpeech, speakAsync, speakVoiceLineAsync)
import Echoic.State
import qualified Graphics.Vty as Vty
import Path (Abs, File, Path)
import Text.Printf (printf)

data AppEvent
  = CommandComplete OutputResult
  deriving (Show)

echoicApp ::
  Config ->
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  BChan AppEvent ->
  App AppState AppEvent ResourceName
echoicApp config voicePath speechVar eventChan =
  App
    { appDraw = drawUI config,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent config voicePath speechVar eventChan,
      appStartEvent = pure (),
      appAttrMap = const theAttrMap
    }

drawUI :: Config -> AppState -> [Widget ResourceName]
drawUI config s = case stateMode s of
  InputMode -> drawInputMode config s
  OutputMode -> drawOutputMode config s

drawInputMode :: Config -> AppState -> [Widget ResourceName]
drawInputMode config s =
  [ vBox $
      [ str "$ " <+> selectedTextCursorWidget MainViewport (stateInputBuffer s),
        str " "
      ]
        <> formatBindingsForUI bindings
  ]
  where
    bindings =
      globalBindingsList (configGlobalBindings config)
        <> inputBindingsList (configInputBindings config)

drawOutputMode :: Config -> AppState -> [Widget ResourceName]
drawOutputMode config s =
  [ vBox $
      case stateOutput s of
        Nothing ->
          [str "No output", str " "]
            <> formatBindingsForUI bindings
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
                str " "
              ]
                <> formatBindingsForUI bindings
  ]
  where
    bindings =
      globalBindingsList (configGlobalBindings config)
        <> outputBindingsList (configOutputBindings config)

-- | Format a bindings list for UI display, one per line
formatBindingsForUI :: [(String, String)] -> [Widget ResourceName]
formatBindingsForUI = map (\(k, d) -> str $ k <> ": " <> d)

-- | Format a bindings list for speech: "key, desc. key, desc."
formatBindingsForSpeech :: [(String, String)] -> String
formatBindingsForSpeech = intercalate ". " . map (\(k, d) -> k <> ", " <> d)

handleEvent ::
  Config ->
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  BChan AppEvent ->
  BrickEvent ResourceName AppEvent ->
  EventM ResourceName AppState ()
handleEvent config voicePath speechVar eventChan event = do
  s <- get
  case event of
    VtyEvent (Vty.EvKey k mods) -> do
      -- Try global bindings first, then mode-specific
      handled <- handleGlobalBindings config voicePath speechVar k mods
      unless handled $
        case stateMode s of
          InputMode -> handleInputMode config voicePath speechVar eventChan k mods
          OutputMode -> handleOutputMode config voicePath speechVar k mods
    AppEvent (CommandComplete result) -> do
      let allLines = resultStdout result <> resultStderr result
          ob =
            OutputBuffer
              { outputLines = if null allLines then ["(no output)"] else allLines,
                outputIndex = 0,
                outputCommand = maybe "" (TextCursor.rebuildTextCursor . stateInputBuffer) (Just s)
              }
          statusVoice =
            if resultExitCode result == 0
              then voiceDone vl
              else voiceFailed vl
      modify $ \st ->
        st
          { stateMode = OutputMode,
            stateOutput = Just ob,
            stateHistory = outputCommand ob : stateHistory st,
            stateInputBuffer = TextCursor.emptyTextCursor
          }
      speakVoice voicePath speechVar statusVoice
    _ -> pure ()
  where
    vl = configVoiceLines config

-- | Check if a key event matches a key combo
matches :: Vty.Key -> [Vty.Modifier] -> KeyCombo -> Bool
matches k mods (KeyCombo k' mods') = k == k' && mods == mods'

-- | Handle global keybindings (work in all modes)
-- Returns True if the key was handled
handleGlobalBindings ::
  Config ->
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  Vty.Key ->
  [Vty.Modifier] ->
  EventM ResourceName AppState Bool
handleGlobalBindings config voicePath speechVar k mods
  | matches k mods (globalCancelSpeech gb) = do
      liftIO $ cancelCurrentSpeech speechVar
      pure True
  | matches k mods (globalListKeys gb) = do
      s <- get
      let description = describeKeysForMode config (stateMode s)
      speakText voicePath speechVar description
      pure True
  | matches k mods (globalSpeedUp gb) = do
      s <- get
      let newSpeed = speedUp (stateVoiceSpeed s)
      modify $ \st -> st {stateVoiceSpeed = newSpeed}
      speakTextWithSpeed voicePath speechVar newSpeed (formatSpeed newSpeed)
      pure True
  | matches k mods (globalSpeedDown gb) = do
      s <- get
      let newSpeed = speedDown (stateVoiceSpeed s)
      modify $ \st -> st {stateVoiceSpeed = newSpeed}
      speakTextWithSpeed voicePath speechVar newSpeed (formatSpeed newSpeed)
      pure True
  | matches k mods (globalQuit gb) = do
      halt
      pure True
  | otherwise = pure False
  where
    gb = configGlobalBindings config

-- | Format speed for speech announcement (as percentage, e.g. "150 percent")
formatSpeed :: Double -> String
formatSpeed s = printf "%.0f percent" (100.0 / s)

-- | Generate a spoken description of available keys for a mode
describeKeysForMode :: Config -> Mode -> String
describeKeysForMode config mode =
  formatBindingsForSpeech (globalBindings <> modeBindings)
  where
    globalBindings = globalBindingsList (configGlobalBindings config)
    modeBindings = case mode of
      InputMode -> inputBindingsList (configInputBindings config)
      OutputMode -> outputBindingsList (configOutputBindings config)

handleInputMode ::
  Config ->
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  BChan AppEvent ->
  Vty.Key ->
  [Vty.Modifier] ->
  EventM ResourceName AppState ()
handleInputMode config voicePath speechVar eventChan k mods
  | matches k mods (inputReadBuffer ib) = do
      s <- get
      let input = TextCursor.rebuildTextCursor (stateInputBuffer s)
      if Text.null input
        then speakVoice voicePath speechVar (voiceEmpty vl)
        else speakText voicePath speechVar (Text.unpack input)
  | matches k mods (inputExecuteCommand ib) = do
      s <- get
      let cmd = TextCursor.rebuildTextCursor (stateInputBuffer s)
      if Text.null cmd
        then speakVoice voicePath speechVar (voiceEmptyCommand vl)
        else do
          speakVoice voicePath speechVar (voiceRun vl)
          liftIO $ do
            _ <- async $ do
              result <- runShellCommand cmd
              writeBChan eventChan (CommandComplete result)
            pure ()
  | matches k mods (inputDeleteBefore ib) =
      modifyBufferDOU TextCursor.textCursorRemove
  | matches k mods (inputDeleteAt ib) =
      modifyBufferDOU TextCursor.textCursorDelete
  | matches k mods (inputMoveCursorLeft ib) =
      modifyBufferM TextCursor.textCursorSelectPrev
  | matches k mods (inputMoveCursorRight ib) =
      modifyBufferM TextCursor.textCursorSelectNext
  -- Character insertion (catch-all for unmodified characters)
  | Vty.KChar c <- k,
    null mods =
      modifyBufferM $ TextCursor.textCursorInsert c
  | otherwise = pure ()
  where
    ib = configInputBindings config
    vl = configVoiceLines config

handleOutputMode ::
  Config ->
  Path Abs File ->
  TVar (Maybe SpeechHandle) ->
  Vty.Key ->
  [Vty.Modifier] ->
  EventM ResourceName AppState ()
handleOutputMode config voicePath speechVar k mods
  | matches k mods (outputReadLine ob) = do
      s <- get
      case stateOutput s of
        Nothing -> pure ()
        Just buf ->
          let idx = outputIndex buf
           in if null (outputLines buf)
                then speakVoice voicePath speechVar (voiceNoOutput vl)
                else speakLine voicePath speechVar (outputLines buf !! idx)
  | matches k mods (outputNextLine ob) = do
      s <- get
      case stateOutput s of
        Nothing -> pure ()
        Just buf ->
          let total = length (outputLines buf)
              idx = outputIndex buf
           in if idx >= total - 1
                then speakVoice voicePath speechVar (voiceBottom vl)
                else do
                  let newIdx = idx + 1
                  modify $ \st -> st {stateOutput = Just buf {outputIndex = newIdx}}
                  speakLine voicePath speechVar (outputLines buf !! newIdx)
  | matches k mods (outputPreviousLine ob) = do
      s <- get
      case stateOutput s of
        Nothing -> pure ()
        Just buf ->
          let idx = outputIndex buf
           in if idx <= 0
                then speakVoice voicePath speechVar (voiceTop vl)
                else do
                  let newIdx = idx - 1
                  modify $ \st -> st {stateOutput = Just buf {outputIndex = newIdx}}
                  speakLine voicePath speechVar (outputLines buf !! newIdx)
  | matches k mods (outputEnterInputMode ob) = do
      modify $ \s -> s {stateMode = InputMode}
      speakVoice voicePath speechVar (voiceInputMode vl)
  | otherwise = pure ()
  where
    ob = configOutputBindings config
    vl = configVoiceLines config

-- | Speak a configured voice line
speakVoice :: Path Abs File -> TVar (Maybe SpeechHandle) -> VoiceLine -> EventM ResourceName AppState ()
speakVoice voicePath speechVar voiceLine = do
  s <- get
  liftIO $ do
    cancelCurrentSpeech speechVar
    handle <- speakVoiceLineAsync voicePath (stateVoiceSpeed s) voiceLine
    atomically $ writeTVar speechVar (Just handle)

-- | Speak arbitrary text (for dynamic content like user input or output lines)
speakText :: Path Abs File -> TVar (Maybe SpeechHandle) -> String -> EventM ResourceName AppState ()
speakText voicePath speechVar text = do
  s <- get
  speakTextWithSpeed voicePath speechVar (stateVoiceSpeed s) text

-- | Speak text at a specific speed (used for speed announcements)
speakTextWithSpeed :: Path Abs File -> TVar (Maybe SpeechHandle) -> Double -> String -> EventM ResourceName AppState ()
speakTextWithSpeed voicePath speechVar speed text = liftIO $ do
  cancelCurrentSpeech speechVar
  handle <- speakAsync voicePath speed text
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
