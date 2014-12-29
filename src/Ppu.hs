-- Picture Processing Unit --

module Ppu (
    createPPU, loadPPU,
    getMem, setMem, PPUState) where

import Data.Word
import Data.Bits
import Data.Int
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as VU
import Control.Applicative hiding ((<|>), many, optional, empty)

data PPU = PPU {
   -- registers :: Registers,
    memory :: Memory, -- ^ PPU contains it's own 16KB RAM
    cycles :: Integer,
    scanLine :: Integer
}

data Memory = Memory (VU.Vector Word8)

type PPUState a = State PPU a

createPPU :: PPU
createPPU = PPU mem 0 0
  where mem = Memory $ VU.replicate 0x4000 0x0

-- Load initial 8KB char map into PPU at addresss 0x0000 - 0x1FFF
loadPPU :: [Word8] -> PPU -> PPU
loadPPU chrs ppu = execState loadMemory' ppu
  where loadMemory' :: PPUState ()
        loadMemory' = do
            mapM_ (\(val, addr) -> setMem val addr) 
                        (zip chrs [0,1..0x1FFF])

getAllMem :: PPUState Memory
getAllMem = get >>= return . memory

setAllMem :: Memory -> PPUState ()
setAllMem mem = get >>= \ppu -> put $ ppu {memory = mem}
      
-- Get/Set word from PPU Memory
getMem :: Word16 -> PPUState Word8
getMem loc = do 
    (Memory mem) <- getAllMem
    return $ mem VU.! (fromIntegral loc)


setMem :: Word8 -> Word16 -> PPUState ()
setMem val addr = do 
    (Memory mem) <- getAllMem
    setAllMem $ Memory $ VU.update mem updatedIndex
 where 
    updatedIndex = VU.fromList $ [(fromIntegral addr, val)] ++ mirrorAddrs
    -- Take care of mirroring, 0x2000 -> 0x2EFF = 0x3000 -> 0x3EFF
    --                         0x3F00 -> 0x3F1F multiple mirrors from 0x3F20 -> 0x3FFF
    mirrorAddrs :: [(Int, Word8)]
    mirrorAddrs = 
        if addr .&. 0xF000 == 0x3000 && addr < 0x3F00 then 
                [(fromIntegral addr - 0x1000, val)]
        else if addr .&. 0xF000 == 0x2000 && addr < 0x2F00  then 
                [(fromIntegral addr + 0x1000, val)]
        else if addr >= 0x3F00 then -- Mirror every 32 bytes (0x20 bytes)
                let offset = addr .&. 0x1F in
                map (\addr -> (fromIntegral $ addr + offset, val)) 
                    (map (\n -> 0x3F00 + (0x20 * n)) [0,1..7])
        else [] -- Not a mirrored address





