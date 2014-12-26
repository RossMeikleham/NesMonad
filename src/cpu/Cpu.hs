-- Atari 2600 6507 CPU --
module Cpu (
    CPU,
    Registers,
    Memory,
    CPUState,
    AddressingMode(..),
    Reg(..),
    getReg, setReg,
    obtainModeVal,
    setModeVal,
    push,
    pop,
    setRegs,
    getRegs,
    setA, setX, setY, setS, setSP, setPC,
    getA, getX, getY, getS, getSP, getPC,
    getIm, setIm,
    getImm, setImm,
    getAllMem, setAllMem,
    getMem, setMem,
    getCarry, getNeg, getZero, getOverflow,
    setFlag,
    setCarry, setNeg, setZero, setOverflow,
    checkNegFlag, checkZeroFlag,
    concatBytesLe, add3, boolToBit
    ) where


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
    Immediate | ZeroPageNoReg | ZeroPage Reg | AbsoluteNoReg |
    Absolute Reg | IndirectX | IndirectY

data Reg = A | B | X | Y | S | SP deriving (Eq)


getReg :: Reg -> CPUState Word8
getReg A = getA
getReg X = getX
getReg Y = getY
getReg S = getS
getReg SP = getSP

setReg :: Reg -> Word8 -> CPUState()
setReg A = setA
setReg X = setX
setReg Y = setY
setReg S = setS
setReg SP = setSP

obtainModeVal mode = case mode of
    Immediate -> getIm
    ZeroPageNoReg -> getMem . fromIntegral =<< getIm
    ZeroPage reg -> getMem . fromIntegral =<< (+) <$> getIm <*> getReg reg
    AbsoluteNoReg -> getMem =<< getImm
    Absolute reg -> getMem =<< (+) <$> getImm <*> (fromIntegral <$> (getReg reg))
    IndirectX -> do
        im <- getIm
        x <- getX 
        addr <- concatBytesLe <$> (getMem $ fromIntegral (im + x)) 
                              <*> (getMem $ fromIntegral (im + x + 1)) 
        getMem addr

    IndirectY -> do 
        im <- getIm
        addr1 <- getMem $ fromIntegral im
        addr2 <- getMem $ fromIntegral (im + 1)
        y <- getY
        getMem $ (concatBytesLe addr1 addr2)  + (fromIntegral y) 

setModeVal :: Word8 -> AddressingMode -> CPUState ()
setModeVal w8 mode = case mode of
    Immediate -> setIm w8
    ZeroPageNoReg -> (setMem w8) . fromIntegral =<< getIm
    ZeroPage reg -> (setMem w8) . fromIntegral =<< (+) <$> getIm <*> getReg reg
    AbsoluteNoReg -> setMem w8 =<< getImm
    Absolute reg -> setMem w8 =<< (+) <$> getImm <*> (fromIntegral <$> (getReg reg))
    IndirectX -> do
        im <- getIm 
        x <- getX
        addr <- concatBytesLe <$> (getMem $ fromIntegral (im + x)) 
                              <*> (getMem $ fromIntegral (im + x + 1)) 
        setMem w8 addr
    IndirectY -> do
        im <- getIm
        addr1 <- getMem $ fromIntegral im
        addr2 <- getMem $ fromIntegral (im + 1)
        y <- getY
        setMem w8 $ (concatBytesLe addr1 addr2)  + (fromIntegral y) 

push :: Word8 -> CPUState () -- [SP] = val, SP = SP - 1
push w8 = do
    addr <- getSP
    setMem w8 $ 0x100 + (fromIntegral addr) 
    setSP (addr - 1)

pop :: CPUState (Word8) -- SP = SP + 1, val = [SP]
pop = do
    addr <- getSP
    val <- getMem $ 1 + 0x100 + (fromIntegral addr) 
    setSP (addr + 1)
    return val 

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

setSP :: Word8 -> CPUState()
setSP w = getRegs >>= \regs -> setRegs $ regs {stackPtr = w}

setPC :: Word16 -> CPUState ()
setPC w = getRegs >>= \regs -> setRegs $ regs {pc = w}

--  |Get Registers
getA :: CPUState Word8
getA = getRegs >>= return . regA

getX :: CPUState Word8
getX = getRegs >>= return . regX

getY :: CPUState Word8
getY = getRegs >>= return . regY

getS :: CPUState Word8
getS = getRegs >>= return . status

getSP :: CPUState Word8
getSP = getRegs >>= return . stackPtr

getPC :: CPUState Word16
getPC = getRegs >>= return . pc
-- Memory

getIm :: CPUState Word8
getIm = do
    pc <- getPC
    getMem (pc + 1)
    
setIm :: Word8 -> CPUState ()
setIm w8 = do
    pc <- getPC
    setMem w8 (pc + 1) 
    
getImm :: CPUState Word16
getImm = do
    pc <- getPC
    concatBytesLe <$> getMem (pc + 1) <*> getMem (pc + 2)

setImm :: Word16 -> CPUState ()
setImm w16 = do
    pc <- getPC
    setMem (fromIntegral (w16 .&. 0x7F)) (pc + 1)
    setMem (fromIntegral (w16 `shiftR` 8)) (pc + 2)

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

getCarry :: CPUState Bool
getCarry = getS >>= \s -> return $ (s .&. 0x1) /= 0

getNeg :: CPUState Bool
getNeg = getS >>= \s -> return $ (s .&. 0x80) /= 0

getZero :: CPUState Bool
getZero = getS >>= \s -> return $ (s .&. 0x2) /= 0

getOverflow :: CPUState Bool
getOverflow = getS >>= \s -> return $ (s .&. 0x40) /= 0


setFlag :: Word8 -> Bool -> CPUState ()
setFlag bits state = do
    s <- getS
    setS $ op s
  where op = if state then (.&. (0xFF - bits)) else (.|. bits)

setCarry :: Bool -> CPUState ()
setCarry = setFlag 0x1 

setNeg :: Bool -> CPUState ()
setNeg = setFlag 0x80

setZero :: Bool -> CPUState ()
setZero = setFlag 0x2

setOverflow :: Bool -> CPUState ()
setOverflow = setFlag 0x40

-- Flag Checking Helper Functions --

checkNegFlag :: Word8 -> CPUState ()
checkNegFlag w8 = setNeg (w8 > 127)

checkZeroFlag :: Word8 -> CPUState ()
checkZeroFlag w8 = setZero (w8 == 0)

boolToBit :: Bool -> Word8
boolToBit b = if b then 1 else 0

-- Concatonate 2 bytes into 16 byte little endian word
-- first byte given is lower half of the word, second is upper half
concatBytesLe :: Word8 -> Word8 -> Word16
concatBytesLe b1 b2 = (fromIntegral b1) .|. (shiftL (fromIntegral b2) 8) 

add3 :: Word8 -> Word8 -> Word8 -> Word8
add3 a b c = a + b + c

