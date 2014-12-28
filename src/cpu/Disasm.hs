-- Dissasembler --

module Disasm (logIns) where

import Instructions (opcodeSizes)
import Cpu

import Data.Word
import Data.Bits
import Control.Monad.State.Strict
import Control.Applicative hiding ((<|>), many, optional, empty)
import Text.Printf

data Instruction = 
      OneWord   String 
    | TwoWord   String Word8 
    | ThreeWord String Word8 Word8
    
data InsType  = 
    None | Imm | ZPage | ZPX | ZPY | Abs | AbsX | AbsY | IndX | IndY | JmpRel | JmpImm | JmpAbs

instructionTable = [
 "BRK","ORA","KIL","*SLO","*NOP","ORA","ASL","*SLO","PHP","ORA","ASL","ANC","*NOP","ORA","ASL","*SLO",
 "BPL","ORA","KIL","*SLO","*NOP","ORA","ASL","*SLO","CLC","ORA","*NOP","*SLO","*NOP","ORA","ASL","*SLO",
 "JSR","AND","KIL","*RLA","BIT","AND","ROL","*RLA","PLP","AND","ROL","ANC","BIT","AND","ROL","*RLA",
 "BMI","AND","KIL","*RLA","*NOP","AND","ROL","*RLA","SEC","AND","*NOP","*RLA","*NOP","AND","ROL","*RLA",
 "RTI","EOR","KIL","*SRE","*NOP","EOR","LSR","*SRE","PHA","EOR","LSR","ALR","JMP","EOR","LSR","*SRE",
 "BVC","EOR","KIL","*SRE","*NOP","EOR","LSR","*SRE","CLI","EOR","*NOP","*SRE","*NOP","EOR","LSR","*SRE",
 "RTS","ADC","KIL","*RRA","*NOP","ADC","ROR","*RRA","PLA","ADC","ROR","ARR","JMP","ADC","ROR","*RRA",
 "BVS","ADC","KIL","*RRA","*NOP","ADC","ROR","*RRA","SEI","ADC","*NOP","*RRA","*NOP","ADC","ROR","*RRA",
 "*NOP","STA","NOP","*SAX","STY","STA","STX","*SAX","DEY","NOP","TXA","XAA","STY","STA","STX","*SAX",
 "BCC","STA","KIL","AHX","STY","STA","STX","*SAX","TYA","STA","TXS","TAS","SHY","STA","SHX","AHX",
 "LDY","LDA","LDX","*LAX","LDY","LDA","LDX","*LAX","TAY","LDA","TAX","LAX","LDY","LDA","LDX","*LAX",
 "BCS","LDA","KIL","*LAX","LDY","LDA","LDX","*LAX","CLV","LDA","TSX","LAS","LDY","LDA","LDX","*LAX",
 "CPY","CMP","NOP","*DCP","CPY","CMP","DEC","*DCP","INY","CMP","DEX","AXS","CPY","CMP","DEC","*DCP",
 "BNE","CMP","KIL","*DCP","*NOP","CMP","DEC","*DCP","CLD","CMP","*NOP","*DCP","*NOP","CMP","DEC","*DCP",
 "CPX","SBC","NOP","*ISB","CPX","SBC","INC","*ISB","INX","SBC","NOP","*SBC","CPX","SBC","INC","*ISB",
 "BEQ","SBC","KIL","*ISB","*NOP","SBC","INC","*ISB","SED","SBC","*NOP","*ISB","*NOP","SBC","INC","*ISB"   
 ]


typeTable = [
 None, IndX, None, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, Abs, Abs, Abs, Abs,
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPX, ZPX, None, AbsY, None, AbsY, AbsX, AbsX, AbsX, AbsX,
 JmpImm, IndX, None, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, Abs, Abs, Abs, Abs,
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPX, ZPX, None, AbsY, None, AbsY, AbsX, AbsX, AbsX, AbsX,
 None, IndX, None, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, JmpImm, Abs, Abs, Abs,
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPX, ZPX, None, AbsY, None, AbsY, AbsX, AbsX, AbsX, AbsX,
 None, IndX, None, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, JmpAbs, Abs, Abs, Abs,
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPX, ZPX, None, AbsY, None, AbsY, AbsX, AbsX, AbsX, AbsX,
 Imm, IndX, Imm, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, Abs, Abs, Abs, Abs,
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPY, ZPY, None, AbsY, None, AbsY, AbsX, AbsX, AbsY, AbsY,
 Imm, IndX, Imm, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, Abs, Abs, Abs, Abs,
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPY, ZPY, None, AbsY, None, AbsY, AbsX, AbsX, AbsY, AbsY,
 Imm, IndX, Imm, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, Abs, Abs, Abs, Abs,  
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPX, ZPX, None, AbsY, None, AbsY, AbsX, AbsX, AbsX, AbsX,
 Imm, IndX, Imm, IndX, ZPage, ZPage, ZPage, ZPage, None, Imm, None, Imm, Abs, Abs, Abs, Abs,
 JmpRel, IndY, None, IndY, ZPX, ZPX, ZPX, ZPX, None, AbsY, None, AbsY, AbsX, AbsX, AbsX, AbsX      
 ] 


-- Disassemble next instruction
disasmCurrent :: CPU -> Instruction
disasmCurrent = evalState disasmCurrent'
  where disasmCurrent' = do
            pc <- getPC
            opcode <- fromIntegral `fmap` getMem pc
            let opcodeSize = opcodeSizes !! opcode
            let ins = instructionTable !! opcode
            operand1 <- getMem (pc + 1)
            operand2 <- getMem (pc + 2)
            return $ case opcodeSize of
                1 -> OneWord   ins
                2 -> TwoWord   ins operand1
                3 -> ThreeWord ins operand1 operand2
                _ -> error $ "Error with instruction size table, should only contain " ++
                           "instructions of sizes 1 - 3"

-- Log current instruction
logIns :: CPU -> String
logIns = evalState logIns'
  where 
    logIns' = do
        pc <- getPC
        opcode <- fromIntegral `fmap` getMem pc
        operand1 <- getMem (pc + 1)
        operand2 <- getMem (pc + 2)
        y <- getY
        let insType = typeTable !! opcode
            insName = instructionTable !! opcode

        (flip (++)) <$> logRegs <*>
          case insType of
            None -> return $ printf "%04X  %02X       %4s %28s" pc opcode insName ""
            Imm ->  return $ printf "%04X  %02X %02X    %4s #$%02X %23s" pc opcode operand1 
                                    insName operand1 ""
            ZPage -> do 
                     val <- obtainModeVal $ ZeroPageNoReg 
                     return $ printf "%04X  %02X %02X    %4s $%02X = %02X %19s" 
                            pc opcode operand1 
                            insName operand1 val "" 
            ZPX -> do 
                     x <- getX
                     val <- obtainModeVal $ ZeroPage X
                     return $ printf "%04X  %02X %02X  %4s $%02X, X @ %02X = %02X %10s" 
                            pc opcode operand1
                            insName operand1 (operand1 + x) val ""   
            ZPY -> do
                    y <- getY
                    val <- obtainModeVal $ ZeroPage Y
                    return $ printf "%04X  %02X %02X  %4s $%02X, Y @ %02X = %02X %10s" 
                            pc opcode operand1
                            insName operand1 (operand1 + y) val ""   
            Abs -> do
                    val <- obtainModeVal AbsoluteNoReg
                    return $ printf "%04X  %02X %02X %02X  %4s $%4X = %02X %10s" 
                            pc opcode operand1
                            operand2 insName (concatBytesLe operand1 operand2) val "" 
            AbsX -> do
                    val <- obtainModeVal $ Absolute X
                    x <- getX
                    let mem = concatBytesLe operand1 operand2
                    return $ printf "%04X  %02X %02X %02X  %4s $%4X,X @ %4X = %02X %10s" 
                            pc opcode operand1
                            operand2 insName (concatBytesLe operand1 operand2) 
                            (mem + (fromIntegral x)) val ""

            AbsY -> do
                    val <- obtainModeVal $ Absolute Y
                    y <- getY
                    let mem = concatBytesLe operand1 operand2
                    return $ printf "%04X  %02X %02X %02X  %4s $%4X,Y @ %4X = %02X %10s" 
                            pc opcode operand1
                            operand2 insName (concatBytesLe operand1 operand2) 
                            (mem + (fromIntegral y)) val ""
            IndX -> do
                    val <- obtainModeVal $ IndirectX
                    x <- getX
                    addr <- concatBytesLe <$> (getMem $ fromIntegral (operand1 + x)) 
                                          <*> (getMem $ fromIntegral (operand1 + x + 1)) 
                    return $ printf "%04X  %02X %02X   %4s ($%02X,X) @ %02X = %04X = %02X   " 
                            pc opcode operand1 insName operand1 (operand1 + x) addr val
            IndY -> do
                    val <- obtainModeVal $ IndirectY
                    y <- getY
                    addr <- concatBytesLe <$> (getMem $ fromIntegral operand1)
                                          <*> (getMem $ fromIntegral (operand1 + 1))
                    return $ printf "%04X  %02X %02X   %4s ($%02X),Y = %04X @ %04X = %02X  "
                            pc opcode operand1 insName operand1 addr 
                            (addr + (fromIntegral y)) val                    

            JmpRel -> do
                    let addr = pc + 2 + (fromIntegral (operand1 .&. 127)) - 
                                        (fromIntegral (operand1 .&. 128)) 
                    return $ printf "%04X  %02X %02X    %4s $%4X %22s" 
                             pc opcode operand1 insName addr ""
 
            JmpImm -> do
                    let addr = concatBytesLe operand1 operand2
                    return $ printf "%04X  %02X %02X %02X %4s $%04X %22s" 
                             pc opcode operand1
                             operand2 insName addr ""

            JmpAbs -> do 
                addr <- getImm 
                val  <- if addr .&. 0xFF /= 0xFF -- Check if page boundary crossed
                            then concatBytesLe <$> getMem addr <*> getMem (addr + 1)
                            else concatBytesLe <$> getMem addr <*> getMem (addr - 0xFF) 
                return $ printf "%04X  %02X %02X %02X %4s ($%04X) = %04X         " 
                         pc opcode operand1
                         operand2 insName addr val

    logRegs = do
        a <- getA
        x <- getX
        y <- getY
        p <- getS
        sp <- getSP
        return $ printf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" a x y p sp
    






