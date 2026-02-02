module Echoic.State
  ( Mode (..),
    AppState (..),
    ResourceName (..),
    initialState,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text

data Mode
  = NormalMode
  | InsertMode
  deriving (Show, Eq)

data ResourceName
  = MainViewport
  deriving (Show, Eq, Ord)

data AppState = AppState
  { stateMode :: !Mode,
    stateInputBuffer :: !Text,
    stateHistory :: ![Text]
  }
  deriving (Show)

initialState :: AppState
initialState =
  AppState
    { stateMode = NormalMode,
      stateInputBuffer = Text.empty,
      stateHistory = []
    }
