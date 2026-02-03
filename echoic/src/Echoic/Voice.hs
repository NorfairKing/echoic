module Echoic.Voice
  ( -- * Voice lines
    VoiceLine (..),
    VoiceLines (..),

    -- * Helpers for defining voice lines
    say,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, File, Path)

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

-- | Create a spoken voice line from a string
say :: String -> VoiceLine
say = VoiceLineSpoken . Text.pack
