import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

merge :: (Ord a) => [a] -> [a] -> [a]
merge l1 l2
    | null l1             = l2 
    | null l2             = l1
    | head l1 <= head l2  = head l1 : merge (tail l1) l2
    | head l1 > head l2   = head l2 : merge l1 (tail l2)


-- Merge sort algo using `merge` from above.`
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []    = []
mergeSort [x]   = [x]
mergeSort [x,y] = merge [x] [y]
mergeSort l     = 
    merge 
        (mergeSort (take (length l `div` 2) l))
        (mergeSort (drop (length l `div` 2) l))


-- This function takes as input the year and 
-- returns a list of all the days in that year.
-- The days are an enum.
daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
    where jan1 = fromGregorian y 1 1
          dec31 = fromGregorian y 12 31


-- Checks if Day (date) is a Friday.
isFriday :: Day -> Bool
isFriday d = dayOfWeek == 5
    where (_, dayOfWeek) = sundayStartWeek d


-- Helper functions from Exercise 2 for checking if prime.
divisors :: Int -> [Int]
divisors n = [ i | i <- [2..(n `div` 2)], n `mod` i == 0 ]


-- New helper function for excluding n <= 1 from counting as prime.
isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = null (divisors n)


-- Checks if date is prime number.
isPrimeDay :: Day -> Bool
isPrimeDay day = isPrime d
    where (_, _, d) = toGregorian day


-- Get list of Days in year that are both prime and fridays.
primeFridays :: Integer -> [Day]
primeFridays y = filter isPrimeDay (filter isFriday (daysInYear y))
