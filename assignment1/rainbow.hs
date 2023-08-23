import RainbowAssign


-- CONSTANTS
pwLength = 8
nLetters = 5


-- Takes a hash in base 10 and converts to base n.
-- e.g. 1726491528 (base 10 integer) -> 12013440212103 (base 5 integer)
convertBase :: Int -> Hash -> [String]
convertBase 0 _        = ["0"]
convertBase base hash = show remainder : convertBase base quotient
    where
        remainder = hash `mod` fromIntegral base
        quotient = hash `div` fromIntegral base


-- Takes string in base n and takes <length> least significant digits.
-- e.g. 12013440212103 -> 40212103
lenLeastSig :: Int -> [String] -> [String]
lenLeastSig len xs = reverse (take len xs)


-- Replaces digits with letters.
-- e.g. 40212103 -> "eacbcbad"
digToLet :: [String] -> Passwd
digToLet = map (toLetter . read) 


-- Composes all the previous functions together.
pwReduce :: Hash -> Passwd
pwReduce = digToLet . lenLeastSig pwLength . convertBase nLetters 


main :: IO ()
main = do
    {-
    pw <- randomPasswords 5 5 10
    print pw
    let hw = map pwHash pw
    print hw
    let rw = map (pwReduce 5 8)  hw
    print rw
    -}
    putStrLn "base: " 
    nLetters <- getLine
    putStrLn "hash: " 
    hash <- getLine
    print (pwReduce (read hash))
