{-# LANGUAGE ScopedTypeVariables #-}

module Echoic.Speech
  ( SpeechHandle,
    speakAsync,
    cancelSpeech,
    isSpeaking,
    speakSync,
  )
where

import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Exception (SomeException, catch)
import Data.ByteString (hGetSome, hPut)
import qualified Data.ByteString as SB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Path (Abs, File, Path, fromAbsFile)
import System.IO (hClose)
import System.Process.Typed
  ( byteStringInput,
    createPipe,
    getStdin,
    getStdout,
    proc,
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
