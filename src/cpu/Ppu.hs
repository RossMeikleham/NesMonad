-- Picture Processing Unit --

module Ppu (createPPU, loadPPU) where

import Data.Word
import Data.Bits
import Data.Int
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as VU
import Control.Applicative hiding ((<|>), many, optional, empty)

data PPU = PPU {
   -- registers :: Registers,
    memory :: Memory, -- ^ PPU contains it's own 64KB RAM
    cycles :: Integer,
    scanLine :: Integer
}

data Memory = Memory (VU.Vector Word8)

type PPUState a = State PPU a

createPPU :: PPU
createPPU = PPU mem 0 0
  where mem = Memory $ VU.replicate 0x10000 0x0

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
setMem val loc = do 
    (Memory mem) <- getAllMem 
    setAllMem $ Memory $ VU.update mem updatedIndex
 where updatedIndex = VU.fromList [(fromIntegral loc, val)]

