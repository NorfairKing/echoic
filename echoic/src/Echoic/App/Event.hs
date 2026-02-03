module Echoic.App.Event
  ( handleEvent,
    EchoicEventM,
    runE,
  )
where

import Brick
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Monad (unless)
import Control.Monad.Reader
import Cursor.Text (TextCursor)
import qualified Cursor.Text as TextCursor
import Cursor.Types (DeleteOrUpdate (..))
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Echoic.App
import Echoic.Config
import Echoic.Env (EchoicEnv (..))
import Echoic.Shell (OutputResult (..), runShellCommand)
import Echoic.Voice.Piper (cancelSpeech, speakAsync, speakVoiceLineAsync)
import qualified Graphics.Vty as Vty
import Text.Printf (printf)

-- | Event handler monad with access to EchoicEnv
type EchoicEventM a = ReaderT EchoicEnv (EventM ResourceName AppState) a

-- | Run an EchoicEventM action
runE :: EchoicEnv -> EchoicEventM a -> EventM ResourceName AppState a
runE = flip runReaderT

handleEvent ::
  Config ->
  BChan AppEvent ->
  BrickEvent ResourceName AppEvent ->
  EchoicEventM ()
handleEvent config eventChan event = do
  s <- lift get
  case event of
    VtyEvent (Vty.EvKey k mods) -> do
      -- Try global bindings first, then mode-specific
      handled <- handleGlobalBindings config k mods
      unless handled $
        case stateMode s of
          InputMode -> handleInputMode config eventChan k mods
          OutputMode -> handleOutputMode config k mods
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
      lift $
        modify $ \st ->
          st
            { stateMode = OutputMode,
              stateOutput = Just ob,
              stateHistory = outputCommand ob : stateHistory st,
              stateInputBuffer = TextCursor.emptyTextCursor
            }
      speakVoice statusVoice
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
  Vty.Key ->
  [Vty.Modifier] ->
  EchoicEventM Bool
handleGlobalBindings config k mods
  | matches k mods (globalCancelSpeech gb) = do
      cancelCurrentSpeech
      pure True
  | matches k mods (globalListKeys gb) = do
      s <- lift get
      let description = describeKeysForMode config (stateMode s)
      speakText description
      pure True
  | matches k mods (globalSpeedUp gb) = do
      newSpeed <- lift $ gets (speedUp . stateVoiceSpeed)
      lift $ modify $ \st -> st {stateVoiceSpeed = newSpeed}
      speakText (formatSpeed newSpeed)
      pure True
  | matches k mods (globalSpeedDown gb) = do
      newSpeed <- lift $ gets (speedDown . stateVoiceSpeed)
      lift $ modify $ \st -> st {stateVoiceSpeed = newSpeed}
      speakText (formatSpeed newSpeed)
      pure True
  | matches k mods (globalQuit gb) = do
      lift halt
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

-- | Format a bindings list for speech: "key, desc. key, desc."
formatBindingsForSpeech :: [(String, String)] -> String
formatBindingsForSpeech = intercalate ". " . map (\(k, d) -> k <> ", " <> d)

handleInputMode ::
  Config ->
  BChan AppEvent ->
  Vty.Key ->
  [Vty.Modifier] ->
  EchoicEventM ()
handleInputMode config eventChan k mods
  | matches k mods (inputReadBuffer ib) = do
      s <- lift get
      let input = TextCursor.rebuildTextCursor (stateInputBuffer s)
      if Text.null input
        then speakVoice (voiceEmpty vl)
        else speakText (Text.unpack input)
  | matches k mods (inputExecuteCommand ib) = do
      s <- lift get
      let cmd = TextCursor.rebuildTextCursor (stateInputBuffer s)
      if Text.null cmd
        then speakVoice (voiceEmptyCommand vl)
        else do
          speakVoice (voiceRun vl)
          liftIO $ do
            _ <- async $ do
              result <- runShellCommand cmd
              writeBChan eventChan (CommandComplete result)
            pure ()
  | matches k mods (inputDeleteBefore ib) =
      lift $ modifyBufferDOU TextCursor.textCursorRemove
  | matches k mods (inputDeleteAt ib) =
      lift $ modifyBufferDOU TextCursor.textCursorDelete
  | matches k mods (inputMoveCursorLeft ib) =
      lift $ modifyBufferM TextCursor.textCursorSelectPrev
  | matches k mods (inputMoveCursorRight ib) =
      lift $ modifyBufferM TextCursor.textCursorSelectNext
  -- Character insertion (catch-all for unmodified characters)
  | Vty.KChar c <- k,
    null mods =
      lift $ modifyBufferM $ TextCursor.textCursorInsert c
  | otherwise = pure ()
  where
    ib = configInputBindings config
    vl = configVoiceLines config

handleOutputMode ::
  Config ->
  Vty.Key ->
  [Vty.Modifier] ->
  EchoicEventM ()
handleOutputMode config k mods
  | matches k mods (outputReadLine ob) = do
      s <- lift get
      case stateOutput s of
        Nothing -> pure ()
        Just buf ->
          let idx = outputIndex buf
           in if null (outputLines buf)
                then speakVoice (voiceNoOutput vl)
                else speakLine (outputLines buf !! idx)
  | matches k mods (outputNextLine ob) = do
      s <- lift get
      case stateOutput s of
        Nothing -> pure ()
        Just buf ->
          let total = length (outputLines buf)
              idx = outputIndex buf
           in if idx >= total - 1
                then speakVoice (voiceBottom vl)
                else do
                  let newIdx = idx + 1
                  lift $ modify $ \st -> st {stateOutput = Just buf {outputIndex = newIdx}}
                  speakLine (outputLines buf !! newIdx)
  | matches k mods (outputPreviousLine ob) = do
      s <- lift get
      case stateOutput s of
        Nothing -> pure ()
        Just buf ->
          let idx = outputIndex buf
           in if idx <= 0
                then speakVoice (voiceTop vl)
                else do
                  let newIdx = idx - 1
                  lift $ modify $ \st -> st {stateOutput = Just buf {outputIndex = newIdx}}
                  speakLine (outputLines buf !! newIdx)
  | matches k mods (outputEnterInputMode ob) = do
      lift $ modify $ \s -> s {stateMode = InputMode}
      speakVoice (voiceInputMode vl)
  | otherwise = pure ()
  where
    ob = configOutputBindings config
    vl = configVoiceLines config

-- | Cancel any currently playing speech
cancelCurrentSpeech :: EchoicEventM ()
cancelCurrentSpeech = do
  speechVar <- asks envSpeechVar
  liftIO $ do
    mHandle <- atomically $ do
      h <- readTVar speechVar
      writeTVar speechVar Nothing
      pure h
    for_ mHandle cancelSpeech

-- | Speak a configured voice line
speakVoice :: VoiceLine -> EchoicEventM ()
speakVoice voiceLine = do
  cancelCurrentSpeech
  speed <- lift $ gets stateVoiceSpeed
  env <- ask
  liftIO $ do
    handle <- speakVoiceLineAsync (envVoicePath env) speed voiceLine
    atomically $ writeTVar (envSpeechVar env) (Just handle)

-- | Speak arbitrary text (for dynamic content like user input or output lines)
speakText :: String -> EchoicEventM ()
speakText text = do
  cancelCurrentSpeech
  speed <- lift $ gets stateVoiceSpeed
  env <- ask
  liftIO $ do
    handle <- speakAsync (envVoicePath env) speed text
    atomically $ writeTVar (envSpeechVar env) (Just handle)

speakLine :: Text -> EchoicEventM ()
speakLine line = speakText (Text.unpack line)

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
