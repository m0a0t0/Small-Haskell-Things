module Uncipher where 
import System.IO
import Data.List (nub, lookup, foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

dictionaryPattern xs = foldl (\acc x -> M.insertWith (++) (wordToPattern x) [x] acc) M.empty $ words $ B.unpack xs
          
wordToPattern xs = foldl (\acc x -> acc ++ [fromMaybe ' ' $ M.lookup x table]) "" xs
    where table = M.fromList $ zip (nub xs) ['a'..'z']

--breakCipher :: [String] -> M.Map String [String] -> [M.Map Char Char]
breakCipher xs dict = breakCipher' xs dict M.empty

breakCipher' :: [String] -> M.Map String [String] -> M.Map Char Char -> [M.Map Char Char]
breakCipher' (x:xs) dict key = foldl' f [] wordsThatFit 
    where wordsThatFit = filter (\y -> wordFitsKey unciphered y key) $ fromMaybe [] $ M.lookup (wordToPattern x) dict
          unciphered = uncipher x key
          f acc y = acc ++ breakCipher' xs dict (f' x y key)
          f' (x':xs') (y:ys) key' = f' xs' ys $ M.insert x' y key'
          f' [] [] key' = key'
breakCipher' [] _ key = [key]

uncipher xs dict = map (\x -> fromMaybe ' ' $ M.lookup x dict) xs

wordFitsKey (x:xs) (y:ys) key
    | or [x  == y, and [x == ' ', y `notElem` (map snd $ M.assocs key)]] = wordFitsKey xs ys key
    | otherwise                                            = False
wordFitsKey [] [] _ = True

