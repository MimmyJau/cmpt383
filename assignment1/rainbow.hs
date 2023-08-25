import qualified Data.Map as Map
import Debug.Trace

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
convertBase 0 _       = ["0"]
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


-- Creates and saves table to file.
generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename


-- Find column in rainbow table.
-- Note: Only called if findChain finds a match.
findNode :: Int -> Passwd -> Hash -> Maybe Passwd
findNode width pass hash
    | width < 0 = Nothing
    | otherwise = if hashString pass == hash
                    then Just pass
                    else findNode (width - 1) ((pwReduce . hashString) pass) hash


-- Find row in rainbow table.
-- Note: Trickier since we don't know "a priori" how many recurses it will take.
-- Note: There may be collisions, so table should check every chain that matches.
findChain :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findChain rainbowTable n hash 
    | n == 0     = Map.lookup hash rainbowTable
    | otherwise  = case Map.lookup hash rainbowTable of 
                     Nothing -> findChain rainbowTable (n - 1) ((hashString . pwReduce) hash)
                     Just value -> case trace (show width ++ show value ++ show hash) $ findNode width value hash of 
                                      Nothing -> findChain rainbowTable (n - 1) ((hashString . pwReduce) hash)
                                      Just p -> Just p


-- Gets table
-- Lookup hash in table
-- If found, done.
-- If not, reduce + hash. Repeat lookup.
-- If found, get password.
-- Reduce + hash until you find the hash. 
    -- Function should return previous passwd iter.

-- Tries to find password in rainbow table.
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword = findChain


debugger :: Passwd -> Int -> Hash
debugger p 0 = trace (show p ++ "," ++ show h) $ hashString p
    where h = hashString p
debugger p n = trace (show p ++ "," ++ show h) debugger ((pwReduce . hashString) p) (n - 1)
    where h = hashString p


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

    

    -- Test writeTable

