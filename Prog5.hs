{- ####################
   CAITLIN HARRIS
   Homework 5
   #################### -}

module Prog5 where
import Data.Char

--1. reverse': function reverses a list using a case expression
reverse' :: [a] -> [a]
reverse' list = case list of
    [] -> []
    (x:xs) -> reverse' xs ++ [x] 


--2. isPalindrome: returns if some list can be read the same way forward and
--backwards ("madam" or "33233")
isPalindrome :: String -> Bool
isPalindrome s = case s of
    [] -> True
    (x:xs) -> reverse' s == s

--3. isLonger: takes two timestamps, if 1st is longer (returns -1)
--if 2nd is longer (returns 1), if equal (returns 0)

--timeStamp type synonym
type TimeStamp = (Int, Int, Int)

isLonger :: TimeStamp -> TimeStamp -> Int
isLonger ts1 ts2
  |ts1 > ts2 = -1
  |ts1 < ts2 = 1
  |ts1 == ts2 = 0

--4. totalSeconds: returns the total seconds
totalSeconds :: TimeStamp -> Int
totalSeconds (h,m,s) = s

--5. isValid: returns whether a time stamp is valid
--non negative && no more than 59 seconds or minutes

--helper: checks if hours, minutes and seconds are non-negative
helper1 :: TimeStamp -> Bool
helper1 (h,m,s) = (h >= 0) && (m >=0) && (s >= 0)

--helper: checks if m and s are less than or equal to 59
helper2 :: TimeStamp -> Bool
helper2 (h,m,s) = (m <= 59) && (s <= 59)

isValid :: TimeStamp -> Bool
isValid (h,m,s) = helper1(h,m,s) && helper2(h,m,s)

--6. time2Str: returns a string representation of timestamp (HH:MM:SS)

--helper: prints a zero in the time stamp if necessary
helper3 :: Int -> String
helper3 x
  |x < 10 = "0" ++ show x
  |otherwise = show x

time2Str :: TimeStamp -> String
time2Str (h,m,s) = helper3 h ++ ":" ++ helper3 m ++ ":" ++ helper3 s

--7. safeFindBefore: takes an item to search for & a list of nums
--returns all the numbers of list before the item is found
safeFindBefore :: Int -> [Int] -> Maybe [Int]
safeFindBefore n [] = Nothing
safeFindBefore n x
  |n == last x = Just (init x)
  |otherwise = safeFindBefore n (init x)


--Set data type
data Set = Set [Int]|EmptySet 
   deriving Show 

--8. check whether given item is present in the given set 
member :: Int -> Set -> Bool
member _ EmptySet = False 
member _ (Set []) = False
member n (Set (x:xs))
  |n == x = True
  |otherwise = member n (Set xs)


--9. size: returns the number of elemnts in a given set
--size :: Set -> Int
size (EmptySet) = 0
size (Set []) = 0
size (Set (x:xs)) = 1 + size (Set xs)

--10. ins: inserts the given element into a set

ins :: Int -> Set -> Set
ins n EmptySet = Set([n])
ins n (Set xs)
  |member n (Set xs) = (Set xs)
  |otherwise = merge n (Set xs)

--helper: merges an Int into a set
merge :: Int -> Set -> Set
merge x (Set (y:ys)) = (Set (x:y:ys))

