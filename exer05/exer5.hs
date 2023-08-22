import Data.Ratio

-- Returns an infite list of repated applications of f of x:
-- e.g. iterate f x = [x, f x, f (f x), ...]
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


-- Returns a tuple where first elem is xs prefix of length n
-- and second elem is the remainder of the list.
-- e.g. splitAt 3 [1,2,3,4,5] = ([1,2,3],[4,5])
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n l = (take n l, drop n l)


-- Returns all rational numbers whose numerator
-- and denominator sum to the argument.
-- e.g. rationalSum 5 = [1 % 4, 2 % 3, 3 % 2, 4 % 1]
rationalSum :: Int -> [Ratio Int]
rationalSum i = [x % y | x <- [1..i], y <- [1..i], x + y == i]


-- Returns same rational numbers as above, but only
-- if they were already in reduced form.
-- e.g. rationalSumLowest 8 = [1 % 7, 3 % 5, 5 % 3, 7 % 1]
rationalSumLowest :: Int -> [Ratio Int]
rationalSumLowest i = [ x % y | 
                        x <- [1..i], 
                        y <- [1..i], 
                        x + y == i,
                        gcd x y == 1 ]


-- Returns a list of all rationals exactly once.
rationals :: [Ratio Int]
rationals = [ r | x <- [1..], r <- rationalSumLowest x ]


-- split a list around a given separator value
splitAtSeparator :: Eq a => a -> [a] -> [[a]]
splitAtSeparator sep [] = []
splitAtSeparator sep content = first : splitAtSeparator sep rest
    where
    first = takeWhile (/= sep) content
    firstlen = length first
    rest = drop (firstlen+1) content


-- convert an integer-like string to an integer
readInt :: String -> Int
readInt = read


-- Take numbers in text file and print their sum.
sumFile :: IO () 
sumFile = do
    content <- readFile "./input.txt"
    let total = sum . map readInt $ splitAtSeparator '\n' content
    print total

