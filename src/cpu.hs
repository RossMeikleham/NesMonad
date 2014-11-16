-- Atari 2600 6507 CPU --

import Data.Word
import Data.Bits
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as VU

data CPU = CPU {
    registers :: Registers,
    memory :: Memory
}

data Registers = Registers {
    regA :: Word8, -- ^ Accumulator
    regX :: Word8, -- ^ Index Register X
    regY :: Word8, -- ^ Index Register Y
    status :: Word8, -- ^ Processor Status Register
    stackPtr :: Word8, -- ^ Stack Pointer
    pc :: Word16 -- ^ Program Counter
}

data StatusFlags = 
      Carry 
    | Zero 
    | IRQDisable
    | Decimal
    | Break
    | Unused
    | Overflow
    | Negative

data Memory = Memory (VU.Vector Word8)

type CPUState a = State CPU a

-- Get/Set all Registers from CPU
getRegs :: CPUState Registers
getRegs = get >>= return . registers

setRegs :: Registers -> CPUState ()
setRegs regs = get >>= \cpu -> put $ cpu {registers = regs}

-- Set specific Registers
setA :: Word8 -> CPUState ()
setA w = getRegs >>= \regs -> setRegs $ regs {regA = w}

setX :: Word8 -> CPUState ()
setX w = getRegs >>= \regs -> setRegs $ regs {regX = w}

setY :: Word8 -> CPUState ()
setY w = getRegs >>= \regs -> setRegs $ regs {regY = w}
     
setS :: Word8 -> CPUState ()
setS w = getRegs >>= \regs -> setRegs $ regs {status = w}


-- Get Registers
getA :: CPUState Word8
getA = getRegs >>= return . regA

getX :: CPUState Word8
getX = getRegs >>= return . regX

getY :: CPUState Word8
getY = getRegs >>= return . regY

getS :: CPUState Word8
getS = getRegs >>= return . status

-- Memory

getAllMem :: CPUState Memory
getAllMem = get >>= return . memory

setAllMem :: Memory -> CPUState ()
setAllMem mem = get >>= \cpu -> put $ cpu {memory = mem}

-- Get/Set word from Memory
getMem :: Word16 -> CPUState Word8
getMem loc = do 
    (Memory mem) <- getAllMem
    return $ mem VU.! (fromIntegral loc)


setMem :: Word8 -> Word16 -> CPUState ()
setMem val loc = do 
    (Memory mem) <- getAllMem 
    setAllMem $ Memory $ VU.update mem updatedIndex
 where updatedIndex = VU.fromList [(fromIntegral loc, val)]

-- Concatonate 2 bytes into 16 byte little endian word
-- first byte given is lower half of the word, second is upper half
concatBytesLe :: Word8 -> Word8 -> Word16
concatBytesLe b1 b2 = (fromIntegral b1) .|. (shiftL (fromIntegral b2) 8) 

executeOpCode :: Word8 -> Word8 -> Word8 -> CPUState ()
executeOpCode op b2 b3
    -- Register/Immediate to Register Transfer 
    | op == 0xA8 = setY =<< getA
    | op == 0xAA = setX =<< getA
    | op == 0xBA = setX =<< getS
    | op == 0x98 = setA =<< getY
    | op == 0x8A = setA =<< getX
    | op == 0x9A = setS =<< getX
    | op == 0xA9 = setA b2
    | op == 0xA2 = setX b2
    | op == 0xA0 = setY b2

    -- Load Register From Memory
    | op == 0xA5 = setA =<< getMem b2'
    | op == 0xB5 = setA =<< (\x -> getMem $ b2' + (fromIntegral x)) =<< getX -- A = [X + nn]
    | op == 0xAD = setA =<< getMem word -- A = [nnnn]
    | op == 0xBD = setA =<< (\x -> getMem $ word + (fromIntegral x)) =<< getX 
    | op == 0xB9 = setA =<< (\y -> getMem $ word + (fromIntegral y)) =<< getY   
    | op == 0xA1 = setA =<< getMem . fromIntegral =<<  -- A = [[X + nn]]
                            (\x -> getMem $ b2' + (fromIntegral x)) =<< getX
    | op == 0xB1 = setA =<< getMem . fromIntegral =<<  -- A = [[X + nn]]
                            (\y -> getMem $ b2' + (fromIntegral y)) =<< getY 
    | op == 0xA6 = setX =<< getMem b2'
    | op == 0xB6 = setX =<< (\x -> getMem $ b2' + (fromIntegral x)) =<< getY -- A = [X + nn]
    | op == 0xAE = setX =<< getMem word -- A = [nnnn]
    | op == 0xBE = setX =<< (\x -> getMem $ word + (fromIntegral x)) =<< getY
    | op == 0xA4 = setY =<< getMem b2'
    | op == 0xB4 = setY =<< (\x -> getMem $ b2' + (fromIntegral x)) =<< getX -- A = [X + nn]
    | op == 0xAC = setY =<< getMem word -- A = [nnnn]
    | op == 0xBC = setY =<< (\x -> getMem $ word + (fromIntegral x)) =<< getX 

 where b2'  = fromIntegral b2
       b3'  = fromIntegral b3
       word = concatBytesLe b2 b3 
       
        


