module Main where

import Test.Framework.Runners.Console (defaultMain)
import PpuTest

main = defaultMain $ [ppuTests]
