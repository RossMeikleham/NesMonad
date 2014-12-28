import Cpu
import Instructions

import qualified Data.ByteString.Lazy as BS 
import Data.Word
import Disasm
import Text.Printf

testFile = "nestest.nes"
outFile = "myResults.log"
startAddr = 0xC000

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
    let cpu = createCPU startAddr
        cpu' = loadMemory rom (startAddr - 16) cpu  
    logCPU 20 cpu'

