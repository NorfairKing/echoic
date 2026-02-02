module Echoic.Shell
  ( OutputResult (..),
    runShellCommand,
  )
where

import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Echoic.State (truncateLine)
import System.Exit (ExitCode (..))
import System.Process.Typed (readProcess, shell)

data OutputResult = OutputResult
  { resultExitCode :: !Int,
    resultStdout :: ![Text],
    resultStderr :: ![Text]
  }
  deriving (Show)

runShellCommand :: Text -> IO OutputResult
runShellCommand cmd = do
  (exitCode, stdout, stderr) <- readProcess (shell (Text.unpack cmd))
  let parseOutput = map truncateLine . Text.lines . TE.decodeUtf8Lenient . LB.toStrict
      exitInt = case exitCode of
        ExitSuccess -> 0
        ExitFailure n -> n
  pure
    OutputResult
      { resultExitCode = exitInt,
        resultStdout = parseOutput stdout,
        resultStderr = parseOutput stderr
      }
