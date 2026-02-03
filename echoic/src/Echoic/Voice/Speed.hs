module Echoic.Voice.Speed
  ( minSpeed,
    maxSpeed,
    speedUp,
    speedDown,
  )
where

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
