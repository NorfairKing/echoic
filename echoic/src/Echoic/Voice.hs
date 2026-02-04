module Echoic.Voice
  ( -- * Voice lines
    VoiceLine (..),
    VoiceLineKey (..),
    VoiceLines (..),
    getVoiceLine,
    defaultVoiceLine,

    -- * Helpers for defining voice lines
    say,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Abs, File, Path)

-- | A voice line can be spoken text or a pre-recorded audio file
data VoiceLine
  = VoiceLineSpoken !Text
  | VoiceLineFile !(Path Abs File)
  deriving (Show)

-- | Keys for all configurable voice lines
data VoiceLineKey
  = Startup
  | Empty
  | EmptyCommand
  | Run
  | Done
  | Failed
  | NoOutput
  | Bottom
  | Top
  | EnterInputMode
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | All configurable voice lines
newtype VoiceLines = VoiceLines (Map VoiceLineKey VoiceLine)
  deriving (Show)

-- | Look up a voice line, falling back to default if not configured
getVoiceLine :: VoiceLineKey -> VoiceLines -> VoiceLine
getVoiceLine key (VoiceLines m) =
  Map.findWithDefault (defaultVoiceLine key) key m

-- | Default voice line for each key
defaultVoiceLine :: VoiceLineKey -> VoiceLine
defaultVoiceLine = \case
  Startup -> say "Echoic ready. Input mode."
  Empty -> say "empty"
  EmptyCommand -> say "empty command"
  Run -> say "run"
  Done -> say "done"
  Failed -> say "failed"
  NoOutput -> say "no output"
  Bottom -> say "bottom"
  Top -> say "top"
  EnterInputMode -> say "input mode"

-- | Create a spoken voice line from a string
say :: String -> VoiceLine
say = VoiceLineSpoken . Text.pack
