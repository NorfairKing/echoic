module Echoic.Env
  ( EchoicM,
    EchoicEnv (..),
    runEchoicM,
    askVoicePath,
    askSpeechVar,
    cancelCurrentSpeech,
    speakTextM,
    speakVoiceLineM,
  )
where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad.Reader
import Data.Foldable (for_)
import Echoic.Voice (VoiceLine)
import Echoic.Voice.Piper (SpeechHandle, cancelSpeech, speakAsync, speakVoiceLineAsync)
import Path

data EchoicEnv = EchoicEnv
  { envVoicePath :: !(Path Abs File),
    envSpeechVar :: !(TVar (Maybe SpeechHandle))
  }

type EchoicM = ReaderT EchoicEnv IO

runEchoicM :: EchoicEnv -> EchoicM a -> IO a
runEchoicM = flip runReaderT

askVoicePath :: EchoicM (Path Abs File)
askVoicePath = asks envVoicePath

askSpeechVar :: EchoicM (TVar (Maybe SpeechHandle))
askSpeechVar = asks envSpeechVar

-- | Cancel any currently playing speech
cancelCurrentSpeech :: EchoicM ()
cancelCurrentSpeech = do
  speechVar <- askSpeechVar
  liftIO $ do
    mHandle <- atomically $ do
      h <- readTVar speechVar
      writeTVar speechVar Nothing
      pure h
    for_ mHandle cancelSpeech

-- | Speak text at a given speed
speakTextM :: Double -> String -> EchoicM ()
speakTextM speed text = do
  cancelCurrentSpeech
  voicePath <- askVoicePath
  speechVar <- askSpeechVar
  liftIO $ do
    handle <- speakAsync voicePath speed text
    atomically $ writeTVar speechVar (Just handle)

-- | Speak a voice line at a given speed
speakVoiceLineM :: Double -> VoiceLine -> EchoicM ()
speakVoiceLineM speed voiceLine = do
  cancelCurrentSpeech
  voicePath <- askVoicePath
  speechVar <- askSpeechVar
  liftIO $ do
    handle <- speakVoiceLineAsync voicePath speed voiceLine
    atomically $ writeTVar speechVar (Just handle)
