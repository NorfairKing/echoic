module Echoic.State
  ( Mode (..),
    OutputBuffer (..),
    AppState (..),
    ResourceName (..),
    initialState,
    maxLineLength,
    truncateLine,
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
    stateHistory :: ![Text]
  }
  deriving (Show)

initialState :: AppState
initialState =
  AppState
    { stateMode = InputMode,
      stateInputBuffer = TextCursor.emptyTextCursor,
      stateOutput = Nothing,
      stateHistory = []
    }

maxLineLength :: Int
maxLineLength = 120

truncateLine :: Text -> Text
truncateLine t
  | Text.length t <= maxLineLength = t
  | otherwise = Text.take (maxLineLength - 3) t <> "..."
