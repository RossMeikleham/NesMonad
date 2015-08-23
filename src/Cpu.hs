-- Atari 2600 6507 CPU --
module Cpu (
    CPU(..),
    createCPU,
    Registers(..),
    Memory,
    CPUState,
    AddressingMode(..),
    Reg(..),
    getReg, setReg,
    obtainModeVal,
    setModeVal,
    push,
    pop,
    setRegs, getRegs,
    setCycles, getCycles, modifyCycles,
    setScanLine, getScanLine, modifyScanLine,
    setA, setX, setY, setS, setSP, setPC,
    getA, getX, getY, getS, getSP, getPC,
    getIm, setIm,
    getImm, setImm,
    getAllMem, setAllMem,
    getMem, setMem,
    getCarry, getNeg, getZero, getOverflow,
    setFlag,
    setCarry, setNeg, setZero, setOverflow, setIRQ,
    checkNegFlag, checkZeroFlag,
    concatBytesLe, add3, boolToBit, isNeg
    ) where


import Data.Word
import Data.Bits
import Data.Int
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as VU


data CPU = CPU {
    registers :: Registers,
    memory :: Memory,
    cycles :: Integer,
    scanLine :: Integer
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

data Memory = Memory {
    workRAM :: VU.Vector Word8, -- ^ 0x0000 - 0x07FF Work RAM
    ppuCtrlRAM :: VU.Vector Word8, -- ^ 0x2000 - 0x2007 PPU Control Registers
    apuRegisters :: VU.Vector Word8, -- ^ 0x4000 - 0x401F  APU Registers
    expansionROM :: VU.Vector Word8, -- ^ 0x4200 - 0x5FFF Cartridge Expansion ROM
    staticRAM :: VU.Vector Word8, -- ^ 0x6000 - 0x7FFF SRAM
    prgROM0 :: VU.Vector Word8, -- ^ 0x8000 - 0xBFFF Program ROM bank 0
    prgROM1 :: VU.Vector Word8 -- ^ 0xC000 - 0xFFFF Program ROM bank 1
}

type CPUState a = State CPU a

data AddressingMode = 
    Immediate | ZeroPageNoReg | ZeroPage Reg | AbsoluteNoReg |
    Absolute Reg | IndirectX | IndirectY 

data Reg = A | X | Y | S | SP deriving (Eq)

createCPU :: Word16 -> CPU
createCPU startAddr = cpu'
  where cycles' = 0
        regs = Registers 0 0 0 0x24 0xFD startAddr
        cpu' = CPU regs mem cycles' 241
        mem = Memory 
            (VU.replicate 0x0800 0x0)
            (VU.replicate 0x0008 0x0)
            (VU.replicate 0x0020 0x0)
            (VU.replicate 0x1FDF 0x0)
            (VU.replicate 0x2000 0x0)
            (VU.replicate 0x4000 0x0)
            (VU.replicate 0x4000 0x0)

-- Load ROM into memory
--loadROM :: [Word8] -> CPU -> CPU
--loadROM rom cpu = cpu {memory = {prgROM0 = romReg, prgROM1 = romRec}} 
--  where romVec = VU.fromList rom
     
      
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


-- Check if memory access crosses page boundary e.g. 0x01FF 0x0200 are in different pages
-- if so, add 1 to current cycle count
checkPageBoundary :: Word16 -> Word16 -> CPUState ()
checkPageBoundary w1 w2 = 
    if (w1 .&. 0xFF00) /= (w2 .&. 0xFF00)
        then modifyCycles (+1)
    else
        return()

-- Obtain value in memoru for given addressing mode, 
-- If boundary check is set then for Absolute X and Y, and Indirect Y check
-- if page boundary crosses and increment the cycle count if it has
obtainModeVal :: AddressingMode -> Bool -> CPUState Word8
obtainModeVal mode checkBoundary = case mode of
    Immediate -> getIm

    ZeroPageNoReg -> getMem . fromIntegral =<< getIm

    ZeroPage reg -> getMem . fromIntegral =<< (+) <$> getIm <*> getReg reg

    AbsoluteNoReg -> getMem =<< getImm

    Absolute reg -> do
        r <- getReg reg
        imm <- getImm 
        let sum' = imm + (fromIntegral r) 
        when checkBoundary $ checkPageBoundary imm sum'
        getMem sum'

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
        let addr = concatBytesLe addr1 addr2
        y <- fromIntegral `fmap` getY
        when checkBoundary $ checkPageBoundary addr (addr + y)
        getMem $ addr  + y

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
    -- SP points to mem locations 0x100 to 0x1FF 
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

-- Get/Set Cycles of CPU
getCycles :: CPUState Integer
getCycles = get >>= return . cycles

setCycles :: Integer -> CPUState ()
setCycles c = get >>= \cpu -> put $ cpu {cycles = c}


modifyCycles :: (Integer -> Integer) -> CPUState () 
modifyCycles f = do
    cycles' <- getCycles 
    setCycles (f cycles')

-- Get/Set current ScanLine
getScanLine :: CPUState Integer
getScanLine = get >>= return . scanLine

setScanLine :: Integer -> CPUState ()
setScanLine sl = get >>= \cpu -> put $ cpu {scanLine = sl}

modifyScanLine :: (Integer -> Integer) -> CPUState ()
modifyScanLine f = do
    sl <- getScanLine
    setScanLine (f sl)

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
    pc' <- getPC
    getMem (pc' - 1)
    
setIm :: Word8 -> CPUState ()
setIm w8 = do
    pc' <- getPC
    setMem w8 (pc' - 1) 
    
getImm :: CPUState Word16
getImm = do
    pc' <- getPC
    concatBytesLe <$> getMem (pc' - 2) <*> getMem (pc' - 1)

setImm :: Word16 -> CPUState ()
setImm w16 = do
    pc' <- getPC
    setMem (fromIntegral (w16 .&. 0x7F)) (pc' - 2)
    setMem (fromIntegral (w16 `shiftR` 8)) (pc' - 1)

getAllMem :: CPUState Memory
getAllMem = get >>= return . memory

setAllMem :: Memory -> CPUState ()
setAllMem mem = get >>= \cpu -> put $ cpu {memory = mem}

-- Get/Set word from Memory
getMem :: Word16 -> CPUState Word8
getMem addr = do 
    (Memory wRAM pCtrlRAM aRegisters expansionRegs static prgR0 prgR1) <- getAllMem
    if addr < 0x2000 then
        return $ wRAM VU.! fromIntegral (addr .&. 0x7FF)
    else if addr < 0x4000 then
        return $ pCtrlRAM VU.! fromIntegral (addr .&. 0x7)
    else if addr < 0x4020 then 
        return $ aRegisters VU.! fromIntegral (addr - 0x4000) 
    else if addr < 0x6000 then
        return $ expansionRegs VU.! fromIntegral (addr - 0x4020)
    else if addr < 0x8000 then
        return $ static VU.! fromIntegral (addr - 0x6000)
    else if addr < 0xC000 then
        return $ prgR0 VU.! fromIntegral (addr - 0x8000)
    else 
        return $ prgR1 VU.! fromIntegral (addr - 0xC000)


setMem :: Word8 -> Word16 -> CPUState ()
setMem val addr = do 
    mem@(Memory wRAM pCtrlRAM aRegisters expansionRegs static prgR0 prgR1) <- getAllMem
    if addr < 0x2000 then
        setAllMem $ mem {workRAM = setMem' wRAM (.&. 0x7FF)}
    else if addr < 0x4000 then
        setAllMem $ mem {ppuCtrlRAM = setMem' pCtrlRAM (.&. 0x7)}
    else if addr < 0x4020 then 
        setAllMem $ mem {apuRegisters = setMem' aRegisters (\a -> a - 0x4000)}
    else if addr < 0x6000 then
        setAllMem $ mem {expansionROM = setMem' expansionRegs (\a -> a - 0x4020)}
    else if addr < 0x8000 then
        setAllMem $ mem {staticRAM = setMem' static (\a -> a - 0x6000)}
    else if addr < 0xC000 then
        setAllMem $ mem {prgROM0 = setMem' prgR0 (\a -> a - 0x8000)}
    else 
        setAllMem $ mem {prgROM1 = setMem' prgR1 (\a -> a - 0xC000)}
    
    where setMem' :: VU.Vector Word8 -> (Word16 -> Word16) -> VU.Vector Word8
          setMem' v f = VU.update v (VU.fromList [(fromIntegral (f addr), val)])


getCarry :: CPUState Bool
getCarry = getS >>= \s -> return $ (s .&. 0x1) /= 0

getNeg :: CPUState Bool
getNeg = getS >>= \s -> return $ (s .&. 0x80) /= 0

getZero :: CPUState Bool
getZero = getS >>= \s -> return $ (s .&. 0x2) /= 0

getOverflow :: CPUState Bool
getOverflow = getS >>= \s -> return $ (s .&. 0x40) /= 0


setFlag :: Word8 -> Bool -> CPUState ()
setFlag bits state' = do
    s <- getS
    setS $ op s
  where op = if state' then (.|. bits) else (.&. (0xFF - bits)) 

setCarry :: Bool -> CPUState ()
setCarry = setFlag 0x1 

setNeg :: Bool -> CPUState ()
setNeg = setFlag 0x80

setZero :: Bool -> CPUState ()
setZero = setFlag 0x2

setOverflow :: Bool -> CPUState ()
setOverflow = setFlag 0x40

setIRQ :: Bool -> CPUState ()
setIRQ = setFlag 0x4

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
concatBytesLe b1 b2 = fromIntegral b1 .|. shiftL (fromIntegral b2) 8

add3 :: Word8 -> Word8 -> Word8 -> Word8
add3 a b c = a + b + c

isNeg :: Word8 -> Bool
isNeg = (>) 127

toSigned :: Word8 -> Int8
toSigned w8 = fromIntegral $ (w8 .&. 127) - fromIntegral (w8 .&. 128)

