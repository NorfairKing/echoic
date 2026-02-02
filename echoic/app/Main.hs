module Main (main) where

import Echoic (runEchoic)
import Echoic.Default (defaultConfig)

main :: IO ()
main = runEchoic defaultConfig
