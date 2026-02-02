module Echoic (runEchoic) where

import Data.ByteString (hGetSome, hPut)
import qualified Data.ByteString as SB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Echoic.OptParse
import Path
import System.IO (hClose)
import System.Process.Typed

runEchoic :: IO ()
runEchoic = do
  Settings {..} <- getSettings
  speak settingVoicePath "Hello, world. Echoic is ready."

speak :: Path Abs File -> String -> IO ()
speak voicePath text = do
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
  where
    streamPipe src dst = do
      chunk <- hGetSome src 4096
      if SB.null chunk
        then hClose dst
        else do
          hPut dst chunk
          streamPipe src dst
