module Echoic.Config
  ( -- * Re-exports from Keybinding
    module Echoic.Keybinding,

    -- * Re-exports from Voice
    module Echoic.Voice,

    -- * Complete config
    Config (..),
  )
where

import Echoic.Keybinding
import Echoic.Voice

-- | Complete configuration
data Config = Config
  { configGlobalBindings :: !GlobalBindings,
    configInputBindings :: !InputBindings,
    configOutputBindings :: !OutputBindings,
    configVoiceLines :: !VoiceLines
  }
  deriving (Show)
