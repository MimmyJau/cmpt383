divisors :: Int -> [Int]
divisors n = [ i | i <- [2..(n `div` 2)], n `mod` i == 0 ]

primes :: Int -> [Int]
-- Should use `null (divisors i)` instead of `i == []`.
primes n = [ i | i <- [2..n], divisors i == [] ]


pythagorean :: Int -> [ (Int, Int, Int) ]
pythagorean n = [ (x, y, z) | z <- [1..n], y <- [1..z], x <- [1..y], x*x + y*y == z*z ]


join :: String -> [String] -> String
join a [] = []
join a [x] = x
join a (x:lst)  = x ++ a ++ join a lst


-- Could use (*) as first param to foldl, 
-- but want to make explicit function argument.
fact' :: Int -> Int
fact' a = foldl (\acc x -> acc * x) 1 [1..a]


-- Hailstone sequence (from last week's exercises).
hailstone :: Int -> Int
hailstone n 
    | even n    = n `div` 2
    | otherwise = 3 * n  + 1


{- Here's a broken-up implementation for clarity
 -
-- Tail Recursive Hailstone
hailTail :: Int -> Int -> Int
hailTail acc 1 = acc
hailTail acc n = hailTail (acc + 1) (hailstone n)


-- Why does this work like `hailLen n = hailTail 0 n`?
-- A function that take two arguments is really a function that applies the first 
-- argument to itself, returning a function, then applying the second argument to 
-- the new function.
-- Thus, hailTail 0 returns a function that is exactly what we want for hailLen.
hailLen :: Int -> Int
hailLen = hailTail 0

-}

hailLen :: Int -> Int
hailLen n = hailTail 0 n
    where
        hailTail a 1 = a
        hailTail a n = hailTail (a + 1) (hailstone n)
