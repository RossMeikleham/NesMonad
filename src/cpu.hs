-- Atari 2600 6507 CPU --

import Data.Word
import Data.Bits
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as VU
import Control.Applicative hiding ((<|>), many, optional, empty)

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

data AddressingMode = 
    Implied Reg | Immediate | ZeroPageNoReg | ZeroPage Reg | AbsoluteNoReg |
    Absolute Reg | IndirectX | IndirectY

data Reg = A | B | X | Y | S 


getReg :: Reg -> CPUState Word8
getReg A = getA
getReg X = getX
getReg Y = getY
getReg S = getS

setReg :: Reg -> Word8 -> CPUState()
setReg A = setA
setReg X = setX
setReg Y = setY
setReg S = setS

-- Load Instruction
loadIns :: Reg -> AddressingMode -> CPUState ()
loadIns r1 (Implied r2) = setReg r1 =<< getReg r2 -- r1 = r2

loadIns r  Immediate = do -- r = nn
    pc <- getPC
    imm <- getMem $ pc + 1
    setReg r imm

loadIns r  ZeroPageNoReg = do -- r = [nn]
    pc <- getPC
    val <- getMem . fromIntegral =<< getMem (pc + 1)
    setReg r val

loadIns r1 (ZeroPage r2) = do -- r1 = [nn + r2]
    pc <- getPC
    rval <- getReg r2
    val <- getMem . fromIntegral . (rval +)  =<< getMem (pc + 1)
    setReg r1 val

loadIns r AbsoluteNoReg = do -- r = [nnnn]
    pc <- getPC
    val <- getMem =<< concatBytesLe <$> getMem (pc + 1) <*> getMem (pc + 2)
    setReg r val

loadIns r1 (Absolute r2) = do -- r = [nnnn + r2]
    pc <- getPC
    r2val <- fromIntegral <$> getReg r2
    nnnn <- concatBytesLe <$> getMem (pc + 1) <*> getMem (pc + 2)
    val <- getMem $ nnnn + r2val
    setReg r1 val

loadIns r IndirectX = do -- r = [[nn + X]]
    pc <- getPC
    x <- fromIntegral <$> getX
    nn <- fromIntegral <$> getMem (pc + 1)
    val <- getMem . fromIntegral =<< getMem (nn + x)
    setReg r val

loadIns r IndirectY = do -- r = [[nn] + Y]
    pc <- getPC
    y <- fromIntegral <$> getY
    nnMem <- fromIntegral <$> (getMem . fromIntegral =<< getMem (pc + 1))
    val <- getMem (nnMem + y)
    setReg r val

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

setPC :: Word16 -> CPUState ()
setPC w = getRegs >>= \regs -> setRegs $ regs {pc = w}

-- Get Registers
getA :: CPUState Word8
getA = getRegs >>= return . regA

getX :: CPUState Word8
getX = getRegs >>= return . regX

getY :: CPUState Word8
getY = getRegs >>= return . regY

getS :: CPUState Word8
getS = getRegs >>= return . status

getPC :: CPUState Word16
getPC = getRegs >>= return . pc
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


executeOpCode :: Word8 -> CPUState ()
executeOpCode op
    -- Register/Immediate to Register Transfer 
    
    | op == 0xA8 = loadIns Y (Implied A)
    | op == 0xAA = loadIns X (Implied A)
    | op == 0xBA = loadIns X (Implied S)
    | op == 0x98 = loadIns A (Implied Y)
    | op == 0x8A = loadIns A (Implied X)
    | op == 0x9A = loadIns S (Implied X)
    | op == 0xA9 = loadIns A Immediate
    | op == 0xA2 = loadIns X Immediate
    | op == 0xA0 = loadIns Y Immediate

    -- Load Register From Memory
    
    | op == 0xA5 = loadIns A ZeroPageNoReg 
    | op == 0xB5 = loadIns A (ZeroPage X) 
    | op == 0xAD = loadIns A AbsoluteNoReg
    | op == 0xBD = loadIns A (Absolute X)
    | op == 0xB9 = loadIns A (Absolute Y) 
    | op == 0xA1 = loadIns A IndirectX 
    | op == 0xB1 = loadIns A IndirectY 

    | op == 0xA6 = loadIns X ZeroPageNoReg
    | op == 0xB6 = loadIns X (ZeroPage Y)
    | op == 0xAE = loadIns X AbsoluteNoReg
    | op == 0xBE = loadIns X (Absolute Y)
    
    | op == 0xA4 = loadIns Y ZeroPageNoReg 
    | op == 0xB4 = loadIns Y (ZeroPage Y)
    | op == 0xAC = loadIns Y AbsoluteNoReg
    | op == 0xBC = loadIns Y (Absolute Y)
    
       
        


