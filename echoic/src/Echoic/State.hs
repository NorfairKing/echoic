module Echoic.State
  ( Mode (..),
    OutputBuffer (..),
    AppState (..),
    ResourceName (..),
    initialState,
    maxLineLength,
    truncateLine,
    speedUp,
    speedDown,
    minSpeed,
    maxSpeed,
  )
where

import Cursor.Text (TextCursor)
import qualified Cursor.Text as TextCursor
import Data.Text (Text)
import qualified Data.Text as Text

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

initialState :: AppState
initialState =
  AppState
    { stateMode = InputMode,
      stateInputBuffer = TextCursor.emptyTextCursor,
      stateOutput = Nothing,
      stateHistory = [],
      stateVoiceSpeed = 1.0
    }

maxLineLength :: Int
maxLineLength = 120

truncateLine :: Text -> Text
truncateLine t
  | Text.length t <= maxLineLength = t
  | otherwise = Text.take (maxLineLength - 3) t <> "..."

-- | Speed limits (lower value = faster speech)
minSpeed, maxSpeed :: Double
minSpeed = 0.4
maxSpeed = 2.0

-- | Speed adjustment step
speedStep :: Double
speedStep = 0.1

-- | Increase speech speed (decrease length scale)
speedUp :: Double -> Double
speedUp s = max minSpeed (s - speedStep)

-- | Decrease speech speed (increase length scale)
speedDown :: Double -> Double
speedDown s = min maxSpeed (s + speedStep)
