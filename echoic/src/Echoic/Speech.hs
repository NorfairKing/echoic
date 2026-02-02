{-# LANGUAGE ScopedTypeVariables #-}

module Echoic.Speech
  ( SpeechHandle,
    speakAsync,
    speakVoiceLineAsync,
    cancelSpeech,
    isSpeaking,
    speakSync,
    speakVoiceLineSync,
  )
where

import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Exception (SomeException, catch)
import Data.ByteString (hGetSome, hPut)
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Echoic.Config (VoiceLine (..))
import Path (Abs, File, Path, fromAbsFile)
import System.IO (hClose)
import System.Process.Typed
  ( byteStringInput,
    createPipe,
    getStdin,
    getStdout,
    proc,
    runProcess_,
    setStdin,
    setStdout,
    stopProcess,
    withProcessWait_,
  )

newtype SpeechHandle = SpeechHandle (Async ())

speakAsync :: Path Abs File -> String -> IO SpeechHandle
speakAsync voicePath text = do
  thread <- async $ speakSync voicePath text
  pure (SpeechHandle thread)

-- | Speak a voice line asynchronously
speakVoiceLineAsync :: Path Abs File -> VoiceLine -> IO SpeechHandle
speakVoiceLineAsync voicePath voiceLine = do
  thread <- async $ speakVoiceLineSync voicePath voiceLine
  pure (SpeechHandle thread)

cancelSpeech :: SpeechHandle -> IO ()
cancelSpeech (SpeechHandle thread) =
  cancel thread `catch` \(_ :: SomeException) -> pure ()

isSpeaking :: SpeechHandle -> IO Bool
isSpeaking (SpeechHandle thread) = do
  result <- poll thread
  pure $ case result of
    Nothing -> True
    Just _ -> False

speakSync :: Path Abs File -> String -> IO ()
speakSync voicePath text = do
  let textBytes = LTE.encodeUtf8 (LT.pack text)

      piperProc =
        setStdin (byteStringInput textBytes) $
          setStdout createPipe $
            proc "piper" ["--model", fromAbsFile voicePath, "--output-raw"]

      aplayProc =
        setStdin createPipe $
          proc "aplay" ["-r", "22050", "-f", "S16_LE", "-t", "raw", "-q"]

  withProcessWait_ piperProc $ \piper ->
    withProcessWait_ aplayProc $ \aplay ->
      streamPipe (getStdout piper) (getStdin aplay)
        `catch` \(_ :: SomeException) -> do
          stopProcess piper
          stopProcess aplay
  where
    streamPipe src dst = do
      chunk <- hGetSome src 4096
      if SB.null chunk
        then hClose dst
        else do
          hPut dst chunk
          streamPipe src dst

-- | Speak a voice line synchronously
speakVoiceLineSync :: Path Abs File -> VoiceLine -> IO ()
speakVoiceLineSync voicePath voiceLine = case voiceLine of
  VoiceLineSpoken text -> speakSyncText voicePath text
  VoiceLineFile audioFile -> playAudioFile audioFile

-- | Speak text using TTS
speakSyncText :: Path Abs File -> Text -> IO ()
speakSyncText voicePath text = speakSync voicePath (Text.unpack text)

-- | Play an audio file directly
playAudioFile :: Path Abs File -> IO ()
playAudioFile audioFile =
  runProcess_ $ proc "aplay" ["-q", fromAbsFile audioFile]
