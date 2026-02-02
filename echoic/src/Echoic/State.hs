module Echoic.State
  ( Mode (..),
    AppState (..),
    ResourceName (..),
    initialState,
  )
where

import Cursor.Text (TextCursor)
import qualified Cursor.Text as TextCursor
import Data.Text (Text)

data Mode
  = NormalMode
  | InsertMode
  deriving (Show, Eq)

data ResourceName
  = MainViewport
  deriving (Show, Eq, Ord)

data AppState = AppState
  { stateMode :: !Mode,
    stateInputBuffer :: !TextCursor,
    stateHistory :: ![Text]
  }
  deriving (Show)

initialState :: AppState
initialState =
  AppState
    { stateMode = NormalMode,
      stateInputBuffer = TextCursor.emptyTextCursor,
      stateHistory = []
    }
