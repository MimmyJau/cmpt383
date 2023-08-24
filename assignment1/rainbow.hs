import qualified Data.Map as Map

import RainbowAssign


-- CONSTANTS
pwLength, nLetters, width, height :: Int
filename :: FilePath

pwLength = 8
nLetters = 5
width = 40
height = 1000
filename = "table.txt"


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


-- Recursively apply hash + reduce to create chain.
-- 1) Edge condition applies hash once to password (i.e. n = 0).
-- 2) Recursion applies reduce then hash to the keys. 
-- Note: Data structure is funky in that the hash is in the key position 
-- and the password is in the value position. This is so that we can
-- more easily lookup the hash, but it's intuitive cause we don't typically
-- picture a chain "going" from right-to-left.
rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable 0 xs = Map.fromList $ zip hashes xs
    where hashes = map hashString xs
rainbowTable n xs = Map.mapKeys (hashString. pwReduce) (rainbowTable (n - 1) xs) 



generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename


main :: IO ()
main = do
    {-
    -- Test RainbowAssign functions
    pw <- randomPasswords 5 5 10
    print pw
    let hw = map pwHash pw
    print hw
    let rw = map (pwReduce 5 8)  hw
    print rw

    -- Test reduce
    putStrLn "base: " 
    nLetters <- getLine
    putStrLn "hash: " 
    hash <- getLine
    print (pwReduce (read hash))
    -}

    -- Test rainbowTable
    print (rainbowTable 40 ["abcdeabc", "aabbccdd", "eeeeeeee"])
    print (rainbowTable 2 ["dccdecee","cdeccaed","acbcaeec","eeeeaebd","ccdccbeb"])
    print (rainbowTable width ["acdgcddh","fcfeggeh","ebfeecbe"])
