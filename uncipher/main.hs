import Uncipher
import System.IO
import Control.Monad (mapM_)
import qualified Data.ByteString.Char8 as B

main = do
    h <- openFile "wordsEn.txt" ReadMode
    conts <- B.hGetContents h
    let dict = dictionaryPattern conts
--    let ciphered = "jrypbzr gb zl jbaqreshy pvcure qrpbqre"
    let ciphered = "ejp mysljylc kd kxveddknmc re jsicpdrysi rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd de kr kd eoya kw aej tysr re ujdr lkgc jv"
    let keys = breakCipher (words ciphered) dict
    putStrLn ciphered
    putStrLn "our language is impossible to understand there are twenty six factorial possibilities so it is okay if you want to just give up"
    mapM_ (\x -> putStrLn $ uncipher ciphered x) keys
--    putStrLn $ uncipher ciphered (head keys)
    hClose h
