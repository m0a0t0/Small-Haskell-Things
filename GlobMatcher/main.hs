import GlobMatcher (matchGlob)

main = do
    putStrLn "f??.c foo.c"
    print $ matchGlob "f??.c" "foo.c"
    putStrLn "f??.c fooo.c"
    print $ matchGlob "f??.c" "fooo.c"
    putStrLn "f*.c foo.c"
    print $ matchGlob "f*.c" "foo.c"
    putStrLn "* abcdefg"
    print $ matchGlob "*" "abcdefg"
