module Main where

import Data.ByteString (hGetSome, hPut)
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hClose)
import System.Process.Typed

main :: IO ()
main = do
  voicePath <-
    lookupEnv "PIPER_VOICE" >>= \case
      Nothing -> die "PIPER_VOICE environment variable not set. Run from nix develop shell."
      Just p -> pure p

  speak voicePath "Hello, world. Echoic is ready."

speak :: FilePath -> String -> IO ()
speak voicePath text = do
  let textBytes = TLE.encodeUtf8 (TL.pack text)

      piperProc =
        setStdin (byteStringInput textBytes) $
          setStdout createPipe $
            proc "piper" ["--model", voicePath, "--output-raw"]

      aplayProc =
        setStdin createPipe $
          proc "aplay" ["-r", "22050", "-f", "S16_LE", "-t", "raw", "-q"]

  withProcessWait_ piperProc $ \piper ->
    withProcessWait_ aplayProc $ \aplay ->
      streamPipe (getStdout piper) (getStdin aplay)
  where
    streamPipe src dst = do
      chunk <- hGetSome src 4096
      if BS.null chunk
        then hClose dst
        else do
          hPut dst chunk
          streamPipe src dst
