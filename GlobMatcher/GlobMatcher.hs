module GlobMatcher (matchGlob) where

matchGlob :: String -> String -> Bool
matchGlob ('*':[]) ys = True
matchGlob pat@('*':x:xs) (y:ys)
    | matchGlob [x] [y] = matchGlob xs ys
    | otherwise         = matchGlob pat ys
matchGlob ('?':xs) (y:ys) = matchGlob xs ys
matchGlob ('[':xs) (y:ys) = characterClass xs (y:ys)
matchGlob ('\\':x:xs) (y:ys)
    | x `elem` escape = if x == y then matchGlob xs ys else False
    | otherwise       = if '\\' == y then matchGlob (x:xs) ys else False
matchGlob (x:xs) (y:ys)
    | x == y    = matchGlob xs ys
    | otherwise = False
matchGlob "" "" = True
matchGlob "" _ = False
matchGlob _ "" = False

escape = "[]*?"

characterClass ('!':xs) (y:ys)
    | not $ characterClass' xs y = matchGlob (charsAfterCharClass xs) ys
    | otherwise                  = False
characterClass xs (y:ys)
    | characterClass' xs y = matchGlob (charsAfterCharClass xs) ys
    | otherwise            = False

characterClass' (x:'-':x2:']':xs) y = y `elem` [x..x2]
characterClass' xs y = y `elem` (charsInCharClass xs) 

charsInCharClass xs = takeWhile (/= ']') xs
charsAfterCharClass xs = tail $ dropWhile (/= ']') xs
