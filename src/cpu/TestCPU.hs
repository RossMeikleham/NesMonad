import Cpu
import Instructions

import qualified Data.ByteString.Lazy as BS 
import Data.Word
import Data.Bits
import Debugger
import Text.Printf

testFile = "nestest.nes"
outFile = "myResults.log"
startAddr = 0xC000

data MirrorType =  Horizontal | Vertical

-- 16 Byte Header sta the start of NES ROM
data NESHeader = NESHeader {
    correctFormat :: Bool, -- ^ Header is correct format
    prgROMBanks :: Int, -- ^ Number of 16KB Programmable ROM banks
    vROMBanks :: Int, -- ^ Number of 8KB Character/VROM banks
    mirrorType :: MirrorType, -- ^ Type of mirroring used by game
    batteryRAM :: Bool, -- ^ ROM contains battery backed RAM    
    trainer :: Bool, -- ^ ROM contains 512 byte trainer at mem locations $7000 -$71FF
    mirror4 :: Bool, -- ^ Whether or not 4 screen mirrorning should be used
    ramBanks :: Int  -- ^ Number of 8KB RAM banks
}

createHeader :: [Word8] -> NESHeader
createHeader rom = NESHeader cf romBanks vBanks mirror 
                             battery train fourScreenMirror ramBks
  where
    cf = (take 4 rom) == [0x4E, 0x45, 0x53, 0x1A]
    romBanks = fromIntegral $ rom !! 4
    vBanks = fromIntegral $ rom !! 5
    mirror = if romControlByte1 `testBit` 0 
                then Horizontal
                else Vertical
    battery = romControlByte1 `testBit` 1
    train = romControlByte1 `testBit` 2
    fourScreenMirror = romControlByte1 `testBit` 3
    ramBks = fromIntegral $ rom !! 8             
    romControlByte1 = rom !! 6


getTestRom :: IO [Word8]
getTestRom = do
    rom <- BS.readFile testFile
    return $ BS.unpack rom

logCPU :: Integer -> CPU -> IO ()
logCPU i cpu 
    | i < 1 = return ()
    | otherwise = do
        appendFile outFile $ logIns cpu ++ "\n" 
       -- printf "%X\n" $ pc $ registers $ cpu
        logCPU (i - 1) (stepInstruction cpu)


main = do 
    writeFile outFile ""
    rom <- getTestRom
    let pRom = take 0x4000 $ drop 16 $ rom
    let cpu = createCPU startAddr
        cpu' = loadMemory pRom startAddr $ loadMemory pRom 0x8000 cpu
        
    --print "starting"
    logCPU 8991 cpu'

