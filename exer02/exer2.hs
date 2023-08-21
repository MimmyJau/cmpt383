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

