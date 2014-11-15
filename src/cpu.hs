-- Atari 2600 6507 CPU --

import Data.Word
import Control.Monad.State.Lazy

data CPU = CPU {
    registers :: Registers
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


type CPUState a = State CPU a

getRegs :: CPUState Registers
getRegs = do
    cpu <- get
    return (registers cpu)

setRegs :: Registers -> CPUState ()
setRegs regs = do
    cpu <- get
    put (cpu {registers = regs})

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


tay = 0xA8
tax = 0xAA
tsx = 0xBA
tya = 0x98
txa = 0x8A
txs = 0x9A


executeOpCode :: Word8 -> CPUState ()
executeOpCode op  
    | op == tay = setY =<< getA
    | op == tax = setX =<< getA
    | op == tsx = setX =<< getS
    | op == tya = setA =<< getY
    | op == txa = setA =<< getX
    | op == txs = setS =<< getX



