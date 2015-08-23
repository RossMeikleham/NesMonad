-- Unit tests for Cpu --

module CpuTest (cpuTests) where

import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Word
import Control.Monad.State.Strict
import Data.Bits

import Cpu

cpu = createCPU 0x0000

-- Check 0x0000 -> 0x0800 is mirrored correctly
testWorkRamMirror :: TFA.Test
testWorkRamMirror = 
    TFA.testGroup "CPU Work RAM mirror" tests
  where
    tests = map (\(addr, val, label) -> testCase (show label) $
        let res = evalState (setMirror addr val) cpu in
            sequence_ (map (\r -> assertEqual ("addr " ++ show addr) val r) res)) 
            (zip3 addrs vals ([1..] :: [Int]))

    vals = [0xA9, 0xFF, 0x00, 0xDD, 0x2F, 0x44, 0x98, 0xCB]
    addrs = [0x000, 0x892, 0x1FFF, 0x1754, 0x1BCD, 0x0569, 0x3FC, 0xACD]
    
    setMirror :: Word16 -> Word8 -> CPUState [Word8]
    setMirror addr val = do
        setMem val addr 
        let offset = addr .&. 0x7FF
        mapM (\n -> getMem (offset + (0x800 * n))) [0,1..3] 


-- Check 0x2000 -> 0x2007 is mirrored correctly
testCtrlRamMirror :: TFA.Test
testCtrlRamMirror = 
    TFA.testGroup "CPU Control RAM mirror" tests
  where
    tests = map (\(addr, val, label) -> testCase (show label) $
        let res = evalState (setMirror addr val) cpu in
            sequence_ (map (\r -> assertEqual ("addr " ++ show addr) val r) res)) 
            (zip3 addrs vals ([1..] :: [Int]))

    vals = [0x9A, 0xFF, 0x00, 0xB3, 0x82, 0xAF, 0x90, 0xDA]
    addrs = [0x3F00, 0x3F3F, 0x3F49, 0x3F72, 0x3F8B, 0x3FBB, 0x3FC1, 0x3FFF]
    
    setMirror :: Word16 -> Word8 -> CPUState [Word8]
    setMirror addr val = do
        setMem val addr 
        let offset = addr .&. 0x07
        mapM (\n -> getMem (0x2000 + offset + (0x08 * n))) [0,1..1023] 



cpuTests = TFA.testGroup "CPU Tests" 
    [testWorkRamMirror, testCtrlRamMirror]
