module Echoic.Config
  ( -- * Key combinations
    KeyCombo (..),

    -- * Keybinding records
    InputBindings (..),
    OutputBindings (..),

    -- * Voice lines
    VoiceLine (..),
    VoiceLines (..),

    -- * Complete config
    Config (..),

    -- * Helpers for defining key combos
    key,
    char,
    ctrl,

    -- * Helpers for defining voice lines
    say,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import Path (Abs, File, Path)

-- | Complete configuration
data Config = Config
  { configInputBindings :: !InputBindings,
    configOutputBindings :: !OutputBindings,
    configVoiceLines :: !VoiceLines
  }
  deriving (Show)

-- | Keybindings for input mode (command entry)
data InputBindings = InputBindings
  { inputReadBuffer :: !KeyCombo,
    inputCancelSpeech :: !KeyCombo,
    inputExecuteCommand :: !KeyCombo,
    inputDeleteBefore :: !KeyCombo,
    inputDeleteAt :: !KeyCombo,
    inputMoveCursorLeft :: !KeyCombo,
    inputMoveCursorRight :: !KeyCombo
  }
  deriving (Show)

-- | Keybindings for output mode (output navigation)
data OutputBindings = OutputBindings
  { outputReadLine :: !KeyCombo,
    outputNextLine :: !KeyCombo,
    outputPreviousLine :: !KeyCombo,
    outputCancelSpeech :: !KeyCombo,
    outputEnterInputMode :: !KeyCombo,
    outputQuit :: !KeyCombo
  }
  deriving (Show)

-- | A key combination: a key plus zero or more modifiers
data KeyCombo = KeyCombo
  { keyComboKey :: !Vty.Key,
    keyComboMods :: ![Vty.Modifier]
  }
  deriving (Show, Eq)

-- | A voice line can be spoken text or a pre-recorded audio file
data VoiceLine
  = VoiceLineSpoken !Text
  | VoiceLineFile !(Path Abs File)
  deriving (Show)

-- | All configurable voice lines
data VoiceLines = VoiceLines
  { voiceStartup :: !VoiceLine,
    voiceEmpty :: !VoiceLine,
    voiceEmptyCommand :: !VoiceLine,
    voiceRun :: !VoiceLine,
    voiceDone :: !VoiceLine,
    voiceFailed :: !VoiceLine,
    voiceNoOutput :: !VoiceLine,
    voiceBottom :: !VoiceLine,
    voiceTop :: !VoiceLine,
    voiceInputMode :: !VoiceLine
  }
  deriving (Show)

-- | Create a key combo from a special key (no modifiers)
key :: Vty.Key -> KeyCombo
key k = KeyCombo k []

-- | Create a key combo from a character (no modifiers)
char :: Char -> KeyCombo
char c = KeyCombo (Vty.KChar c) []

-- | Create a key combo from a character with Ctrl modifier
ctrl :: Char -> KeyCombo
ctrl c = KeyCombo (Vty.KChar c) [Vty.MCtrl]

-- | Create a spoken voice line from a string
say :: String -> VoiceLine
say = VoiceLineSpoken . Text.pack
