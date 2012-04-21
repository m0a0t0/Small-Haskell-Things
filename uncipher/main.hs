import Uncipher
import System.IO
import Control.Monad (mapM_)
import qualified Data.ByteString.Char8 as B

main = do
    h <- openFile "wordsEn.txt" ReadMode
    conts <- B.hGetContents h
    let dict = dictionaryPattern conts
    let ciphered = "jrypbzr gb zl jbaqreshy pvcure qrpbqre"
    let keys = breakCipher (words ciphered) dict
    putStrLn ciphered
    putStrLn "welcome to my wonderful cipher decoder"
    mapM_ (\x -> putStrLn $ uncipher ciphered x) keys
--    putStrLn $ uncipher ciphered (head keys)
    hClose h
