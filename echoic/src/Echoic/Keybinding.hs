{-# LANGUAGE LambdaCase #-}

module Echoic.Keybinding
  ( -- * Key combinations
    KeyCombo (..),
    showKeyCombo,

    -- * Keybinding records
    GlobalBindings (..),
    InputBindings (..),
    OutputBindings (..),

    -- * Bindings as lists (for UI and speech)
    globalBindingsList,
    inputBindingsList,
    outputBindingsList,

    -- * Helpers for defining key combos
    key,
    char,
    ctrl,
    ctrlShift,
  )
where

import qualified Graphics.Vty as Vty

-- | A key combination: a key plus zero or more modifiers
data KeyCombo = KeyCombo
  { keyComboKey :: !Vty.Key,
    keyComboMods :: ![Vty.Modifier]
  }
  deriving (Show, Eq)

-- | Keybindings that work in every mode
data GlobalBindings = GlobalBindings
  { globalCancelSpeech :: !KeyCombo,
    globalListKeys :: !KeyCombo,
    globalQuit :: !KeyCombo,
    globalSpeedUp :: !KeyCombo,
    globalSpeedDown :: !KeyCombo
  }
  deriving (Show)

-- | Keybindings for input mode (command entry)
data InputBindings = InputBindings
  { inputReadBuffer :: !KeyCombo,
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
    outputEnterInputMode :: !KeyCombo
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

-- | Create a key combo from a character with Ctrl+Shift modifiers
ctrlShift :: Char -> KeyCombo
ctrlShift c = KeyCombo (Vty.KChar c) [Vty.MCtrl, Vty.MShift]

-- | Show a key combo in human-readable format
showKeyCombo :: KeyCombo -> String
showKeyCombo (KeyCombo k mods) =
  concatMap showMod mods <> showKey k
  where
    showMod = \case
      Vty.MCtrl -> "Ctrl+"
      Vty.MMeta -> "Meta+"
      Vty.MAlt -> "Alt+"
      Vty.MShift -> "Shift+"
    showKey = \case
      Vty.KChar c -> [c]
      Vty.KEsc -> "Esc"
      Vty.KEnter -> "Enter"
      Vty.KBS -> "Backspace"
      Vty.KDel -> "Delete"
      Vty.KLeft -> "Left"
      Vty.KRight -> "Right"
      Vty.KUp -> "Up"
      Vty.KDown -> "Down"
      Vty.KHome -> "Home"
      Vty.KEnd -> "End"
      Vty.KPageUp -> "PageUp"
      Vty.KPageDown -> "PageDown"
      _ -> "?"

-- | Convert global bindings to a list of (key, description) pairs
globalBindingsList :: GlobalBindings -> [(String, String)]
globalBindingsList gb =
  [ (showKeyCombo (globalCancelSpeech gb), "cancel"),
    (showKeyCombo (globalListKeys gb), "keys"),
    (showKeyCombo (globalSpeedUp gb) <> "/" <> showKeyCombo (globalSpeedDown gb), "speed"),
    (showKeyCombo (globalQuit gb), "quit")
  ]

-- | Convert input bindings to a list of (key, description) pairs
inputBindingsList :: InputBindings -> [(String, String)]
inputBindingsList ib =
  [ (showKeyCombo (inputReadBuffer ib), "read"),
    (showKeyCombo (inputExecuteCommand ib), "run"),
    (showKeyCombo (inputDeleteBefore ib), "delete before"),
    (showKeyCombo (inputDeleteAt ib), "delete at cursor"),
    (showKeyCombo (inputMoveCursorLeft ib), "move left"),
    (showKeyCombo (inputMoveCursorRight ib), "move right")
  ]

-- | Convert output bindings to a list of (key, description) pairs
outputBindingsList :: OutputBindings -> [(String, String)]
outputBindingsList ob =
  [ (showKeyCombo (outputReadLine ob), "read"),
    (showKeyCombo (outputNextLine ob) <> "/" <> showKeyCombo (outputPreviousLine ob), "navigate"),
    (showKeyCombo (outputEnterInputMode ob), "input")
  ]
