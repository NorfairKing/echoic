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
    nullStream,
    proc,
    runProcess_,
    setStderr,
    setStdin,
    setStdout,
    stopProcess,
    withProcessWait_,
  )

newtype SpeechHandle = SpeechHandle (Async ())

speakAsync :: Path Abs File -> Double -> String -> IO SpeechHandle
speakAsync voicePath speed text = do
  thread <- async $ speakSync voicePath speed text
  pure (SpeechHandle thread)

-- | Speak a voice line asynchronously
speakVoiceLineAsync :: Path Abs File -> Double -> VoiceLine -> IO SpeechHandle
speakVoiceLineAsync voicePath speed voiceLine = do
  thread <- async $ speakVoiceLineSync voicePath speed voiceLine
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

speakSync :: Path Abs File -> Double -> String -> IO ()
speakSync voicePath speed text = do
  let textBytes = LTE.encodeUtf8 (LT.pack text)
      -- Convert speed (length-scale style: lower=faster) to tempo (higher=faster)
      tempoFactor = 1.0 / speed

      piperProc =
        setStdin (byteStringInput textBytes) $
          setStdout createPipe $
            setStderr nullStream $
              proc "piper" ["--model", fromAbsFile voicePath, "--output-raw"]

      -- Sox tempo adjustment: raw 22050Hz 16-bit signed mono -> same format
      soxProc =
        setStdin createPipe $
          setStdout createPipe $
            setStderr nullStream $
              proc
                "sox"
                [ "-t",
                  "raw",
                  "-r",
                  "22050",
                  "-e",
                  "signed",
                  "-b",
                  "16",
                  "-c",
                  "1",
                  "-",
                  "-t",
                  "raw",
                  "-",
                  "tempo",
                  show tempoFactor
                ]

      aplayProc =
        setStdin createPipe $
          setStderr nullStream $
            proc "aplay" ["-r", "22050", "-f", "S16_LE", "-t", "raw", "-q"]

  withProcessWait_ piperProc $ \piper ->
    withProcessWait_ soxProc $ \sox ->
      withProcessWait_ aplayProc $ \aplay -> do
        -- Stream piper -> sox in background
        piperToSox <- async $ streamPipe (getStdout piper) (getStdin sox)
        -- Stream sox -> aplay in foreground
        streamPipe (getStdout sox) (getStdin aplay)
          `catch` \(_ :: SomeException) -> do
            cancel piperToSox
            stopProcess piper
            stopProcess sox
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
speakVoiceLineSync :: Path Abs File -> Double -> VoiceLine -> IO ()
speakVoiceLineSync voicePath speed voiceLine = case voiceLine of
  VoiceLineSpoken text -> speakSyncText voicePath speed text
  VoiceLineFile audioFile -> playAudioFile audioFile

-- | Speak text using TTS
speakSyncText :: Path Abs File -> Double -> Text -> IO ()
speakSyncText voicePath speed text = speakSync voicePath speed (Text.unpack text)

-- | Play an audio file directly
playAudioFile :: Path Abs File -> IO ()
playAudioFile audioFile =
  runProcess_ $ proc "aplay" ["-q", fromAbsFile audioFile]
