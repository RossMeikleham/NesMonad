-- Unit tests for Ppu --

module PpuTest (ppuTests) where

import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Word
import Control.Monad.State.Strict
import Data.Bits

import Ppu

ppu = createPPU

-- Check 0x2000 -> 0x2EFF is mirrored correctly
testNameTableMirrorLower :: TFA.Test
testNameTableMirrorLower = 
    TFA.testGroup "PPU RAM mirror 0x2000 - 0x2EFF" tests
  where
    tests = map (\(addr, val, label) -> testCase (show label) $
        let res = evalState (setMirror addr val) ppu in
        assertEqual ("addr " ++ show addr) val res) (zip3 addrs vals ([1..] :: [Int]))

    vals = [0x1F, 0x3F, 0x94, 0x07]
    addrs = [0x2000, 0x2EFF, 0x2342, 0x2983]

    setMirror :: Word16 -> Word8 -> PPUState Word8 
    setMirror addr val = do
        setMem val addr 
        getMem (addr + 0x1000)    

-- Check 0x3000 -> 0x3FFF is mirrored correctly
testNameTableMirrorHigher :: TFA.Test
testNameTableMirrorHigher = 
    TFA.testGroup "PPU RAM mirror 0x3000 - 0x3EFF" tests
  where
    tests = map (\(addr, val, label) -> testCase (show label) $
        let res = evalState (setMirror addr val) ppu in
        assertEqual ("addr " ++ show addr) val res) (zip3 addrs vals ([1..] :: [Int]))

    vals = [0x63, 0x78, 0x92, 0x26]
    addrs = [0x3000, 0x3EFF, 0x3A8F, 0x342E]

    setMirror :: Word16 -> Word8 -> PPUState Word8 
    setMirror addr val = do
        setMem val addr 
        getMem (addr - 0x1000)    

-- Check pattern mirroring is correct
testPatternMirror :: TFA.Test
testPatternMirror =
    TFA.testGroup "PPU RAM Pattern mirror" tests
  where
    tests = map (\(addr, val, label) -> testCase (show label) $
                let res = evalState (setMirror addr val) ppu in
                sequence_ (map (\r -> assertEqual ("addr " ++ show addr) val r) res)) 
            (zip3 addrs vals ([1..] :: [Int]))

    vals = [0x8A, 0xFF, 0x78, 0xB1, 0x45, 0x72, 0x92, 0xD3]
    addrs = [0x3F00, 0x3F3F, 0x3F49, 0x3F72, 0x3F8B, 0x3FBB, 0x3FC1, 0x3FFF]
    
    setMirror :: Word16 -> Word8 -> PPUState [Word8]
    setMirror addr val = do
        setMem val addr 
        let offset = addr .&. 0x1F
        mapM (\n -> getMem (0x3F00 + offset + (0x20 * n))) [0,1..7] 
    

-- Check mirroring doesn't occur when it shouldn't
--testNoMirroring :: TFA.Test
--testNoMirroring =
--    TFA.testGroup "No incorrect mirroring checks" tests
--  where
--    test1 = 



ppuTests = TFA.testGroup "PPU Tests" 
    [testNameTableMirrorLower, testNameTableMirrorHigher, testPatternMirror]

