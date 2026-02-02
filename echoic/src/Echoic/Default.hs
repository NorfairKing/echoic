module Echoic.Default
  ( defaultConfig,
    defaultGlobalBindings,
    defaultInputBindings,
    defaultOutputBindings,
    defaultVoiceLines,
  )
where

import Echoic.Config
import qualified Graphics.Vty as Vty

-- | Default configuration for Echoic
defaultConfig :: Config
defaultConfig =
  Config
    { configGlobalBindings = defaultGlobalBindings,
      configInputBindings = defaultInputBindings,
      configOutputBindings = defaultOutputBindings,
      configVoiceLines = defaultVoiceLines
    }

-- | Default keybindings that work in every mode
--
-- Esc: cancel speech
-- ?: list available keys
-- Ctrl+Shift+D: quit
defaultGlobalBindings :: GlobalBindings
defaultGlobalBindings =
  GlobalBindings
    { globalCancelSpeech = key Vty.KEsc,
      globalListKeys = char '?',
      globalQuit = ctrl 'd'
    }

-- | Default keybindings for input mode
--
-- Ctrl+R: read current input
-- Enter: execute command
-- Backspace: delete before cursor
-- Delete: delete at cursor
-- Left: move cursor left
-- Right: move cursor right
defaultInputBindings :: InputBindings
defaultInputBindings =
  InputBindings
    { inputReadBuffer = ctrl 'r',
      inputExecuteCommand = key Vty.KEnter,
      inputDeleteBefore = key Vty.KBS,
      inputDeleteAt = key Vty.KDel,
      inputMoveCursorLeft = key Vty.KLeft,
      inputMoveCursorRight = key Vty.KRight
    }

-- | Default keybindings for output mode
--
-- .: read current line
-- j: next line
-- k: previous line
-- i: enter input mode
defaultOutputBindings :: OutputBindings
defaultOutputBindings =
  OutputBindings
    { outputReadLine = char '.',
      outputNextLine = char 'j',
      outputPreviousLine = char 'k',
      outputEnterInputMode = char 'i'
    }

-- | Default voice lines
defaultVoiceLines :: VoiceLines
defaultVoiceLines =
  VoiceLines
    { voiceStartup = say "Echoic ready. Input mode.",
      voiceEmpty = say "empty",
      voiceEmptyCommand = say "empty command",
      voiceRun = say "run",
      voiceDone = say "done",
      voiceFailed = say "failed",
      voiceNoOutput = say "no output",
      voiceBottom = say "bottom",
      voiceTop = say "top",
      voiceInputMode = say "input mode"
    }
