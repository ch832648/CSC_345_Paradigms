{- ####################
   Caitlin Harris
   Homework 3
   #################### -}

module Prog3 where
import Data.Char

--1. doubleAll: takes a list of ints and returns a list, first element is the OG 
--number, the second is the OG number doubled
doubleAll :: [Int] -> [(Int, Int)]
doubleAll x = [(x',x'*2) | x' <- x]



--2. productLastPart: takes n and a list of ints, returns the product of the
--last n numbers in the list
productLastPart :: Int -> [Int] -> Int
productLastPart n x = product(take n (reverse x))



--3. init': identical behvior to init: returns all but last
init' :: [Int] -> [Int]
init' x = reverse(tail(reverse(x)))



--4.nestedParens 
nestedParens :: String -> Bool
nestedParens s
  |odd(length(s)) = False
  |s == "()" = True
  |(head s == '(') && (last s == ')') = nestedParens(tail(init s))
  |otherwise = False



--5. triads: generates a list of integer triples
triads :: Int -> [(Int, Int, Int)]
triads n = [(x,y,z) | x <- [1..n], y <- [1..n], z <-[1..n],((x*x) + (y*y)) == z*z]



--6. pushRight: takes a string and integer n, forms a string of length n 
pushRight :: String -> Int -> String
pushRight s n 
  |s == [] = []
  |otherwise = (replicate(n-length(s)) ' ') ++ s



--7. lowerFirstCharacter: takes a string, lowercases the first character
lowerFirstCharacter :: String -> String
lowerFirstCharacter (ch : s) = if (isUpper ch) then (toLower ch : s) else (ch : s)



--8. middleWord: takes a string, returns the middle word
spaces :: String -> [Int]
spaces x = fst(unzip [s | s <- zip[0..length(x)-1] x, snd(s) == ' '])

middle :: String -> Int -> Int -> String
middle xs a' b' = snd(unzip [x | x <- zip[0..length(xs)-1] xs, fst x > a' && fst x < b'])

middleWord :: String -> String
middleWord s = middle s (head(spaces s)) (last(spaces s))



--9. lowerFirstLetter: takes a string, lowers the first uppercase letter found in the string
findCap :: [Char] -> Int
findCap x = head[a | (a,b) <- (zip[0..length x-1] x), isUpper b]

lowerFirstLetter :: String -> String
lowerFirstLetter xs = take(findCap xs) xs ++ lowerFirstCharacter(snd(splitAt(findCap xs) xs))



--10. lowerFirstTwoLetters: takes a string, lowers the first two uppercase letters found in string
lowerFirstTwoLetters :: String -> String
lowerFirstTwoLetters xs = lowerFirstLetter (lowerFirstLetter xs)



