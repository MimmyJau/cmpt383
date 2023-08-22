-- Calculates nth row of Pascal's triangle.
pascal :: Integer -> [Integer]
pascal 0 = [1]
pascal 1 = [1,1]
pascal n = [1] ++ zipWith (+) prev (tail prev) ++ [1]
    where prev = pascal (n - 1)


-- Uncurried version of +.
addPair :: (Int, Int) -> Int
addPair = uncurry (+)


-- Pointfree filtering for eliminating zeros.
-- Need both `Eq a` and `Num a` for compiler
-- to properly deduce type of input.
withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = filter (/= 0) 


-- First intro to Maybe = Just | Nothing.
-- Note: All branches need to return Just or Nothing.
-- Was making a mistake earlier with `x == y = 0`.
findElt :: (Eq a) => a -> [a] -> Maybe Int
findElt x [] = Nothing
findElt x (y:ys) 
    | x == y     = Just 0
    | otherwise  = case findElt x ys of 
                       Nothing -> Nothing
                       Just n -> Just (1 + n)
                                          
