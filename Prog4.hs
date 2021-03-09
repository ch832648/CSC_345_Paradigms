{- ####################
   CAITLIN HARRIS
   Homework 4.
   #################### -}


module Prog4 where
import Data.Char

--1. doubleAll: takes list of ints, returns list of pairs (OG #, OG # doubled)
doubleAll :: [Int] -> [(Int, Int)]
doubleAll [] = []
doubleAll(x:xs) = [(x, x*2)] ++ doubleAll xs


--2. productLastPart: returns the product of the last n numbers in the list
productLastPart :: Int -> [Int] -> Int
productLastPart 0 _ = 1
productLastPart n (x:xs) = last (xs) * productLastPart (n-1) (init xs)


--3. init': has identical behavior to init function
--returns all but the last element in list
init' :: [Int] -> [Int]
init' [x] = []
init' (x:xs) = x : init' xs

--4. lowerOddLetters: lowercases the first, third, and fifth 
lowerOddLetters :: String -> String
lowerOddLetters [] = []
lowerOddLetters [x] = (toLower x) : (lowerOddLetters [])
lowerOddLetters (x:y:xs) = (toLower x) : y : (lowerOddLetters xs)


--5. replicate': has identical behavior to replicate function
--replicates a Char n number of times
replicate' :: Int -> Char -> String
replicate' 0 _ = []
replicate' n c = c : replicate' (n-1) c

--6. iSort': uses insertion sort to sort a list of pairs
ins' :: (Int, String) -> [(Int, String)] -> [(Int, String)]
ins' (a,b) [] = [(a,b)]
ins' x (y:ys)
  |(fst x <= fst y) = x:y:ys
  |otherwise = y:ins' x ys

iSort' :: [(Int, String)] -> [(Int, String)]
iSort' [] = []
iSort' (x:xs) = ins' x (iSort' xs)

--7. lowerFirstCharacter: lowers the first char in a string
lowerFirstCharacter :: String -> String
lowerFirstCharacter (ch:s) = if (isUpper ch) then (toLower ch:s) else (ch:s)

--8. middleWord: returns the second word in a three word string
helper :: String -> String
helper [] = []
helper (x:xs)
  |x == ' ' = middleWord xs
  |otherwise = x : helper xs

middleWord :: String -> String
middleWord [] = []
middleWord (x : xs)
  |x == ' ' = helper xs
  |otherwise = middleWord xs

--9. lowerFirstLetter: lowercases the first uppercase letter in a string
lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x:xs)
  |isUpper x = toLower x : xs ++ lowerFirstLetter []
  |otherwise = x : lowerFirstLetter xs

--10. lowerFirstTwoLetters: lowercases the first two uppercase letters in a string
lowerFirstTwoLetters :: String -> String
lowerFirstTwoLetters xs = lowerFirstLetter (lowerFirstLetter xs)
