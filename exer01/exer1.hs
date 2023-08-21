det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt(det a b c))/2*a
quadsol2 a b c = (-b + sqrt(det a b c))/2*a


-- Use index operator.
third_a  list = list !! 2


-- Use pattern matching.
third_b :: [a] -> a
third_b list = head(tail(tail(list)))

-- Recursive factorial function.
factorial :: Int -> Int
factorial a
    | a <= 1    = 1
    | otherwise = a * factorial(a - 1)


-- Hailstone sequence.
hailstone :: Int -> Int
hailstone n 
    | even n    = n `div` 2
    | otherwise = 3 * n  + 1

-- Hailstone length (# of hailstone operations until we get 1).
hailLen :: Int -> Int
hailLen 1 = 0
hailLen n = 1 + hailLen(hailstone n)
