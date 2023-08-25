import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import RainbowAssign


-- CONSTANTS
pwLength, nLetters, width, height :: Int
filename :: FilePath

pwLength = 8
nLetters = 5
width = 40
height = 10000
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
findNode :: Int -> Hash -> Maybe Passwd -> Maybe Passwd
findNode _ _ Nothing = Nothing
findNode w hash (Just pass)
    | w < 0     = Nothing
    | otherwise = if hashString pass == hash
                    then Just pass
                    else findNode (w - 1) hash ((Just . pwReduce . hashString) pass)



-- Find all matching rows in rainbow table.
-- Note: There may be collisions, so table should check every chain that matches.
findChains :: Map.Map Hash Passwd -> Int -> Hash -> [Maybe Passwd]
findChains table count hash  = filter (/= Nothing) (checkRows count hash)
    where 
        checkRows 0 h = [Map.lookup h table]
        checkRows n h = 
            let nextHash = (hashString . pwReduce) h
            in Map.lookup h table : checkRows (n - 1) nextHash


-- Safely grab head from a [Maybe a] list type.
-- There is not way of safely grabbibg head. Will lead to a runtime error if list is empty.
-- This adds extra pattern of distinguishing Nothing from Just.
-- There is no way of safely grabbing head. Throws a runtime error if list is empty.
-- This function adds extra pattern of distinguishing Nothing from Just.
maybeHead :: [Maybe a] -> Maybe a
maybeHead [] = Nothing
maybeHead (Nothing:xs) = maybeHead xs
maybeHead (Just a:_) = Just a


-- Tries to find password in rainbow table.
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table w hash = mySafeHead $ map (findNode w hash) (findChains table w hash) 


-- Tests pre-generated against n number of passwords.
test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)


main :: IO ()
main = do
    generateTable
    res <- test2 10000
    print res

    


