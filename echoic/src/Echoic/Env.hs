module Echoic.Env
  ( EchoicM,
    EchoicEnv (..),
    runEchoicM,
    askVoicePath,
  )
where

import Control.Monad.Reader
import Path

data EchoicEnv = EchoicEnv
  { envVoicePath :: !(Path Abs File)
  }

type EchoicM = ReaderT EchoicEnv IO

runEchoicM :: EchoicEnv -> EchoicM a -> IO a
runEchoicM = flip runReaderT

askVoicePath :: EchoicM (Path Abs File)
askVoicePath = asks envVoicePath
