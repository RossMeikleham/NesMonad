module Main where

import Test.Framework.Runners.Console (defaultMain)
import PpuTest
import CpuTest

main = defaultMain [cpuTests, ppuTests]
