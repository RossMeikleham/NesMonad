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
    ZeroPageNoReg | ZeroPage Reg | AbsoluteNoReg |
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


-- | Move Reg to Reg
moveRegIns :: Reg -> Reg -> CPUState () 
moveRegIns r1 r2 = setReg r1 =<< getReg r2 -- r1 = r2

loadRegIm :: Reg -> CPUState()
loadRegIm r = do -- r = nn
    pc <- getPC
    imm <- getMem $ pc + 1
    setReg r imm

-- |Load Instruction (Moe Mem to Reg)
loadIns :: Reg -> AddressingMode -> CPUState()
loadIns r  ZeroPageNoReg = setReg r =<< getMem . fromIntegral =<<  getIm -- r = [nn]

loadIns r1 (ZeroPage r2) = do -- r1 = [nn + r2]
    val <- getMem . fromIntegral =<< (+) <$> getReg r2 <*> getIm
    setReg r1 val

loadIns r AbsoluteNoReg = setReg r  =<< getMem =<< getImm -- r = [nnnn]

loadIns r1 (Absolute r2) = do -- r = [nnnn + r2]
    val <- getMem =<< (+) <$> (fromIntegral <$> getReg r2) <*> getImm
    setReg r1 val

loadIns r IndirectX = do -- r = [[nn + X]]
    val <- getMem . fromIntegral =<< getMem . fromIntegral =<< (+) <$> getX <*> getIm
    setReg r val

loadIns r IndirectY = do -- r = [[nn] + Y]
    val <- getMem . fromIntegral =<< (+) <$> (getMem . fromIntegral =<< getIm) <*>  getY
    setReg r val

-- |Store Instruction (Move Reg to Mem)
storeIns :: Reg -> AddressingMode -> CPUState()
storeIns r  ZeroPageNoReg = do -- [nn] = r
    val <- getReg r
    addr <- getIm 
    setMem val (fromIntegral addr)

storeIns r1 (ZeroPage r2) = do --[nn + r2] = r1
    val <- getReg r1
    addr <- (+) <$> getIm <*> getReg r2
    setMem val(fromIntegral addr)

storeIns r AbsoluteNoReg = do -- [nnnn] = r
    val <- getReg r
    addr <- getImm
    setMem val addr

storeIns r1 (Absolute r2) = do -- [nnnn + r2] = r1
    val <- getReg r1
    addr <- (+) <$> getImm <*> (fromIntegral <$> getReg r2)
    setMem val addr

storeIns r IndirectX = do -- [[nn + X]] = r
    val <- getReg r
    addr <- getMem . fromIntegral =<< ((+) <$> getIm <*> getX)
    setMem val (fromIntegral addr)

storeIns r IndirectY = do -- [[nn] + Y] = r
    val <- getReg r
    addr <- (+) <$> getY <*> (getMem . fromIntegral =<< getIm)
    setMem val (fromIntegral addr)


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

--  |Get Registers
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

getIm :: CPUState Word8
getIm = do
    pc <- getPC
    getMem (pc + 1)
    
getImm :: CPUState Word16
getImm = do
    pc <- getPC
    concatBytesLe <$> getMem (pc + 1) <*> getMem (pc + 2)

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
    
    | op == 0xA8 = moveRegIns Y A
    | op == 0xAA = moveRegIns X A
    | op == 0xBA = moveRegIns X S
    | op == 0x98 = moveRegIns A Y
    | op == 0x8A = moveRegIns A X
    | op == 0x9A = moveRegIns S X

    | op == 0xA9 = loadRegIm A 
    | op == 0xA2 = loadRegIm X 
    | op == 0xA0 = loadRegIm Y 

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
    
    -- Store Register into Memory

    | op == 0x85 = storeIns A ZeroPageNoReg        
    | op == 0x95 = storeIns A (ZeroPage X)      
    | op == 0x8D = storeIns A AbsoluteNoReg      
    | op == 0x9D = storeIns A (Absolute X)      
    | op == 0x99 = storeIns A (Absolute Y)      
    | op == 0x81 = storeIns A IndirectX      
    | op == 0x91 = storeIns A IndirectY  
        
    | op == 0x86 = storeIns X ZeroPageNoReg       
    | op == 0x96 = storeIns X (ZeroPage Y)      
    | op == 0x8E = storeIns X AbsoluteNoReg   
       
    | op == 0x84 = storeIns Y ZeroPageNoReg      
    | op == 0x94 = storeIns Y (ZeroPage X)      
    | op == 0x8C = storeIns Y AbsoluteNoReg      
        


