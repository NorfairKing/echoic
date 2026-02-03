module Echoic.App
  ( -- * State types
    Mode (..),
    OutputBuffer (..),
    AppState (..),
    ResourceName (..),
    initialState,

    -- * Events
    AppEvent (..),

    -- * Re-exports from Voice.Speed
    speedUp,
    speedDown,
    minSpeed,
    maxSpeed,
  )
where

import Cursor.Text (TextCursor)
import qualified Cursor.Text as TextCursor
import Data.Text (Text)
import Echoic.Shell (OutputResult)
import Echoic.Voice.Speed (maxSpeed, minSpeed, speedDown, speedUp)

data Mode
  = InputMode
  | OutputMode
  deriving (Show, Eq)

data ResourceName
  = MainViewport
  deriving (Show, Eq, Ord)

data OutputBuffer = OutputBuffer
  { outputLines :: ![Text],
    outputIndex :: !Int,
    outputCommand :: !Text
  }
  deriving (Show)

data AppState = AppState
  { stateMode :: !Mode,
    stateInputBuffer :: !TextCursor,
    stateOutput :: !(Maybe OutputBuffer),
    stateHistory :: ![Text],
    stateVoiceSpeed :: !Double
  }
  deriving (Show)

data AppEvent
  = CommandComplete OutputResult
  deriving (Show)

initialState :: AppState
initialState =
  AppState
    { stateMode = InputMode,
      stateInputBuffer = TextCursor.emptyTextCursor,
      stateOutput = Nothing,
      stateHistory = [],
      stateVoiceSpeed = 1.0
    }
