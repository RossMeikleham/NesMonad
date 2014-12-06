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
    Immediate | ZeroPageNoReg | ZeroPage Reg | AbsoluteNoReg |
    Absolute Reg | IndirectX | IndirectY

data Reg = A | B | X | Y | S | SP


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

-- | Move Reg to Reg
moveRegIns :: Reg -> Reg -> CPUState () 
moveRegIns r1 r2 = setReg r1 =<< getReg r2 -- r1 = r2

-- |Load Instruction (Moe Mem to Reg)
loadIns :: Reg -> AddressingMode -> CPUState()
loadIns reg mode = setReg reg =<< obtainModeVal mode

-- |Store Instruction (Move Reg to Mem)
storeIns :: Reg -> AddressingMode -> CPUState()
storeIns reg mode = (\v -> setModeVal v mode) =<< getReg reg  


-- | Push value from Register into Stack
pushIns ::Reg -> CPUState()
pushIns r  = push =<< getReg r

-- | Pop value from Stack into Register
popIns :: Reg -> CPUState()
popIns r = setReg r =<< pop 
    

-- | ALU Instructions

-- Add with carry value from Accumulator
adcIns :: AddressingMode -> CPUState ()
adcIns mode = setA =<< (add3 <$> carryBit <*> getA <*> n)
  where n = obtainModeVal mode
        carryBit = do 
            carrySet <- getCarry
            return $ boolToBit carrySet

-- Subtract with Carry value from Accumulator
sbcIns :: AddressingMode -> CPUState()
sbcIns mode = setA =<< sub3 <$> ((+) <$> getA <*> carryBit) <*> pure 1 <*> n
  where sub3 a b c = a - b - c
        n = obtainModeVal mode
        carryBit = do 
            carrySet <- getCarry
            return $ boolToBit carrySet

-- AND Accumulator with value
andIns :: AddressingMode -> CPUState()
andIns mode = setA =<< (.&.) <$> getA <*> n
  where n = obtainModeVal mode

-- XOR Accumulator with value
xorIns :: AddressingMode -> CPUState()
xorIns mode = setA =<< xor <$> getA <*> n
    where n = obtainModeVal mode

-- Or Accumulator with value
orIns :: AddressingMode -> CPUState()
orIns mode = setA =<< (.|.) <$> getA <*> n
  where n = obtainModeVal mode

-- Compare register against value, and set appropriate flags
compareIns :: Reg -> AddressingMode -> CPUState()
compareIns _ _ = return () --TODO flags 


bitTestIns :: AddressingMode -> CPUState()
bitTestIns _ = return () -- TODO flags


-- Incrememnt value in register by 1
incReg :: Reg -> CPUState()
incReg reg = setReg reg =<< (+) <$> (getReg reg) <*> (pure 1)

-- Increment value by 1
incIns :: AddressingMode -> CPUState()
incIns mode = do 
    val <- obtainModeVal mode
    setModeVal (val + 1) mode  

-- Decrement value in register by 1
decReg :: Reg -> CPUState()
decReg reg = setReg reg =<< (-) <$> (getReg reg) <*> (pure 1)

-- Decrement value by 1
decIns :: AddressingMode -> CPUState()
decIns mode = do
    val <- obtainModeVal mode
    setModeVal (val - 1) mode


-- shift register left
shiftLReg :: Reg -> CPUState()
shiftLReg reg = setReg reg =<< (shiftL) <$> (getReg reg) <*> (pure 1)

-- Shift left
shiftLIns :: AddressingMode -> CPUState()
shiftLIns mode = do
    val <- obtainModeVal mode
    setModeVal (val `shiftL` 1) mode

-- Shift register right
shiftRReg :: Reg -> CPUState()
shiftRReg reg = setReg reg =<< (shiftR) <$> (getReg reg) <*> (pure 1)

-- Shift right 
shiftRIns :: AddressingMode -> CPUState()
shiftRIns mode = do 
    val <- obtainModeVal mode
    setModeVal (val `shiftR` 1) mode


-- Rotate register Left through carry bit
rotateLReg :: Reg -> CPUState()
rotateLReg reg = setReg reg =<< (+) <$> (shiftL <$> (getReg reg) <*> (pure 1)) <*> carryBit
  where carryBit = getCarry >>= return . boolToBit

-- Rotate Left through carry bit
rotateLIns :: AddressingMode -> CPUState()
rotateLIns mode = do
    val <- obtainModeVal mode
    carry <- carryBit
    setModeVal ((val `shiftL` 1) + carry) mode
  where carryBit = getCarry >>= return . boolToBit

-- Rotate register Right through carry bit
rotateRReg :: Reg -> CPUState()
rotateRReg reg = do
    val <- getReg reg
    carry <- carryBit
    setReg reg  $ (val `shiftR` 1) + (carry * 0x80)
  where carryBit = getCarry >>= return . boolToBit

-- Rotate Right through carry bit
rotateRIns :: AddressingMode -> CPUState()
rotateRIns mode = do
    val <- obtainModeVal mode
    carry <- carryBit
    setModeVal ((val `shiftR` 1) + (carry * 0x80)) mode
  where carryBit = getCarry >>= return . boolToBit

-- Unconditional Jump to Immediate 16 bit address
jmpWordIns :: CPUState()
jmpWordIns = setPC =<< getImm

-- Unconditional Jump to 
jmpMemWordIns :: CPUState()
jmpMemWordIns = do
    addr <- getImm
    if addr .&. 0xFF /= 0xFF -- Check if page boundary crossed
        then setPC =<< concatBytesLe <$> getMem addr <*> getMem (addr + 1)
        -- jmp can't cross page boundaries so msb obtained from start
        -- of page 
        else setPC =<< concatBytesLe <$> getMem addr <*> getMem (addr - 0xFF) 

-- Call instruction, push PC to stack and jump
callIns :: CPUState ()
callIns = do
    pc <- getPC
    push $ fromIntegral $ ((pc - 1) `shiftR` 8)
    push $ fromIntegral $ ((pc - 1) .&. 0xFF)
    addr <- getImm
    setPC addr  

-- Return from Interrupt
retIIns :: CPUState ()
retIIns = do
    p <- pop
    low <- pop
    high <- pop
    setPC $ concatBytesLe low high
    setS p


-- Return from Subroutine
retSIns :: CPUState ()
retSIns = do
    low <- pop
    high <- pop
    setPC $ 1 + concatBytesLe low high
    
-- Branch if not negative
jmpNotNegIns :: CPUState ()
jmpNotNegIns = jmpCond . not =<< getNeg
    
-- Branch if negative 
jmpNegIns :: CPUState ()
jmpNegIns = jmpCond =<< getNeg

-- Branch if not zero
jmpNotZeroIns :: CPUState ()
jmpNotZeroIns = jmpCond . not  =<< getZero 

-- Branch if zero
jmpZeroIns :: CPUState()
jmpZeroIns = jmpCond =<< getZero

-- Branch if no carry
jmpNoCarryIns :: CPUState()
jmpNoCarryIns = jmpCond . not =<< getCarry

-- Branch if carry
jmpCarryIns :: CPUState()
jmpCarryIns = jmpCond =<< getCarry

-- Branch if no overflow
jmpNoOverflowIns :: CPUState()
jmpNoOverflowIns = jmpCond . not =<< getOverflow

-- Branch if overflow
jmpOverflowIns :: CPUState()
jmpOverflowIns = jmpCond =<< getOverflow

-- Branch if condition met, to signed 8 bit offset
jmpCond :: Bool -> CPUState()
jmpCond b = if b then goToOffset else return ()
  where 
    goToOffset = do 
        im <- getIm
        pc <- getPC
        let im' = (fromIntegral im :: Word16)
        setPC $ pc + (im' .&. 127) - (im' .&. 128)

-- Clear carry flag
clearCarryIns :: CPUState () 
clearCarryIns = setS =<< ((0xFF - 0x1) .&.) <$> getS

-- Clear interrupt disable bit
clearIntDisableIns :: CPUState ()
clearIntDisableIns = setS =<< ((0xFF - 0x4) .&.) <$> getS 

-- Clear Decimal mode
clearDecimalIns :: CPUState ()
clearDecimalIns = setS =<< ((0xFF - 0x8) .&.) <$> getS

-- Clear Overflow flag
clearOverflowIns :: CPUState ()
clearOverflowIns = setS =<< ((0xFF - 0x40) .&.) <$> getS

-- Set Carry flag
setCarryIns :: CPUState () 
setCarryIns = setS =<< (0x1 .|.) <$> getS 

-- Set interrupt disable bit
setIntDisableIns :: CPUState ()
setIntDisableIns = setS =<< (0x4 .|.) <$> getS 

-- Set Decimal mode
setDecimalIns :: CPUState ()
setDecimalIns = setS =<< (0x40 .|.) <$> getS 

-- Do nothing nop
nopIns ::CPUState ()
nopIns = return ()

-- | Obtain 8 bit value for given addressing mode
obtainModeVal :: AddressingMode -> CPUState Word8
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

boolToBit :: Bool -> Word8
boolToBit b = if b then 1 else 0

-- Concatonate 2 bytes into 16 byte little endian word
-- first byte given is lower half of the word, second is upper half
concatBytesLe :: Word8 -> Word8 -> Word16
concatBytesLe b1 b2 = (fromIntegral b1) .|. (shiftL (fromIntegral b2) 8) 

add3 :: Word8 -> Word8 -> Word8 -> Word8
add3 a b c = a + b + c

executeOpCode :: Word8 -> CPUState ()
executeOpCode op
    -- Register/Immediate to Register Transfer 
    
    | op == 0xA8 = moveRegIns Y A
    | op == 0xAA = moveRegIns X A
    | op == 0xBA = moveRegIns X S
    | op == 0x98 = moveRegIns A Y
    | op == 0x8A = moveRegIns A X
    | op == 0x9A = moveRegIns S X

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
        
    -- Push/Pop registers from Stack
    | op == 0x48 = pushIns A
    | op == 0x08 = pushIns S
    | op == 0x68 = popIns  A
    | op == 0x28 = popIns  S

    --ALU instructions
    
    | op == 0x69 = adcIns Immediate
    | op == 0x65 = adcIns ZeroPageNoReg
    | op == 0x75 = adcIns (ZeroPage X)
    | op == 0x6D = adcIns AbsoluteNoReg
    | op == 0x7D = adcIns (Absolute X)
    | op == 0x79 = adcIns (Absolute Y)
    | op == 0x61 = adcIns IndirectX
    | op == 0x71 = adcIns IndirectY

    | op == 0xE9 = sbcIns Immediate
    | op == 0xE5 = sbcIns ZeroPageNoReg
    | op == 0xF5 = sbcIns (ZeroPage X)
    | op == 0xED = sbcIns AbsoluteNoReg
    | op == 0xFD = sbcIns (Absolute X)
    | op == 0xF9 = sbcIns (Absolute Y)
    | op == 0xE1 = sbcIns IndirectX
    | op == 0xF1 = sbcIns IndirectY

    | op == 0x29 = andIns Immediate
    | op == 0x25 = andIns ZeroPageNoReg
    | op == 0x35 = andIns (ZeroPage X)
    | op == 0x2D = andIns AbsoluteNoReg
    | op == 0x3D = andIns (Absolute X)
    | op == 0x39 = andIns (Absolute Y)
    | op == 0x21 = andIns IndirectX
    | op == 0x31 = andIns IndirectY

    | op == 0x49 = xorIns Immediate
    | op == 0x45 = xorIns ZeroPageNoReg
    | op == 0x55 = xorIns (ZeroPage X)
    | op == 0x4D = xorIns AbsoluteNoReg
    | op == 0x5D = xorIns (Absolute X)
    | op == 0x59 = xorIns (Absolute Y)
    | op == 0x41 = xorIns IndirectX
    | op == 0x51 = xorIns IndirectY
    
    | op == 0x09 = orIns Immediate
    | op == 0x05 = orIns ZeroPageNoReg
    | op == 0x15 = orIns (ZeroPage X)
    | op == 0x0D = orIns AbsoluteNoReg
    | op == 0x1D = orIns (Absolute X)
    | op == 0x19 = orIns (Absolute Y)
    | op == 0x01 = orIns IndirectX
    | op == 0x11 = orIns IndirectY
    
    | op == 0xC9 = compareIns A Immediate
    | op == 0xC5 = compareIns A ZeroPageNoReg
    | op == 0xD5 = compareIns A (ZeroPage X)
    | op == 0xCD = compareIns A AbsoluteNoReg
    | op == 0xDD = compareIns A (Absolute X)
    | op == 0xD9 = compareIns A (Absolute Y)
    | op == 0xC1 = compareIns A IndirectX
    | op == 0xD1 = compareIns A IndirectY
    | op == 0xE0 = compareIns X Immediate 
    | op == 0xE4 = compareIns X ZeroPageNoReg
    | op == 0xEC = compareIns X AbsoluteNoReg
    | op == 0xC0 = compareIns Y Immediate
    | op == 0xC4 = compareIns Y ZeroPageNoReg
    | op == 0xCC = compareIns Y AbsoluteNoReg

    | op == 0x24 = bitTestIns ZeroPageNoReg
    | op == 0x2C = bitTestIns AbsoluteNoReg

    | op == 0xE6 = incIns ZeroPageNoReg
    | op == 0xF6 = incIns (ZeroPage X)
    | op == 0xEE = incIns AbsoluteNoReg
    | op == 0xFE = incIns (Absolute X)
    | op == 0xE8 = incReg X
    | op == 0xC8 = incReg Y

    | op == 0xC6 = decIns ZeroPageNoReg
    | op == 0xD6 = decIns (ZeroPage X)
    | op == 0xCE = decIns AbsoluteNoReg
    | op == 0xDE = decIns (Absolute X)
    | op == 0xC8 = decReg X
    | op == 0x88 = decReg Y
    
    -- Rotate/Shift instructions
    | op == 0x0A = shiftLReg A
    | op == 0x06 = shiftLIns ZeroPageNoReg
    | op == 0x16 = shiftLIns (ZeroPage X)
    | op == 0x0E = shiftLIns AbsoluteNoReg
    | op == 0x1E = shiftLIns (Absolute X)

    | op == 0x4A = shiftRReg A
    | op == 0x46 = shiftRIns ZeroPageNoReg
    | op == 0x56 = shiftRIns (ZeroPage X)
    | op == 0x4E = shiftRIns AbsoluteNoReg
    | op == 0x5E = shiftRIns (Absolute X)

    | op == 0x2A = rotateLReg A
    | op == 0x26 = rotateLIns ZeroPageNoReg
    | op == 0x36 = rotateLIns (ZeroPage X)
    | op == 0x2E = rotateLIns AbsoluteNoReg
    | op == 0x3E = rotateLIns (Absolute X)

    | op == 0x6A = rotateRReg A
    | op == 0x66 = rotateRIns ZeroPageNoReg
    | op == 0x76 = rotateRIns (ZeroPage X)
    | op == 0x6E = rotateRIns AbsoluteNoReg
    | op == 0x7E = rotateRIns (Absolute X)

    -- Jump/Call instructions
    
    | op == 0x4C = jmpWordIns
    | op == 0x6C = jmpMemWordIns
    | op == 0x20 = callIns
    | op == 0x40 = retIIns 
    | op == 0x60 = retSIns

    | op == 0x10 = jmpNotNegIns
    | op == 0x30 = jmpNegIns
    | op == 0x50 = jmpNoOverflowIns
    | op == 0x70 = jmpOverflowIns
    | op == 0x90 = jmpNoCarryIns
    | op == 0xB0 = jmpCarryIns
    | op == 0xD0 = jmpNotZeroIns
    | op == 0xF0 = jmpCarryIns

    -- Control instructions

    | op == 0x18 = clearCarryIns 
    | op == 0x58 = clearIntDisableIns
    | op == 0xD8 = clearDecimalIns
    | op == 0xB8 = clearOverflowIns
    | op == 0x38 = setCarryIns
    | op == 0x78 = setIntDisableIns
    | op == 0xF8 = setDecimalIns

    | op == 0xEA= nopIns
