 {- ##############################
   Caitlin Harris
   Homework 2
   ############################## -}

module Prog2 where

--threeDifferent: returns true is no 2 args are equal, false otherwise
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = (x /= y) && (y /= z) && (x /= z)


--fourDifferent: returns true if no 2 args are equal, false otherwise
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z = (threeDifferent w x y) && (threeDifferent x y z) && (w /= z)

--sum': recusive function that computes the sum of nums 1 - n
sum' :: Integer -> Integer
sum' n
  |n == 0 = 0
  |n > 0 = n + sum'(n-1)

--asciisum: recursive function that computes the acsii values in a string
asciisum :: String -> Integer
asciisum (s:xs)
  |xs == [] = (fromIntegral(fromEnum s))
  |otherwise = (fromIntegral(fromEnum s)) + (asciisum xs)

--integerSqrt: returns the integer square root of positive num
integerSqrt :: Integer -> Integer
integerSqrt x = floor(sqrt(fromIntegral x))

--minOfThree: returns the minimum integer
minOfThree :: (Integer, Integer, Integer) -> Integer
minOfThree (x, y, z) = fromIntegral(min (min x y) z)

--maxOfThree: returns the maximum integer
maxOfThree :: (Integer, Integer, Integer) -> Integer
maxOfThree (x, y, z) = fromIntegral(max (max x y) z)

--middleOfThree: returns the median integer
middleOfThree :: (Integer, Integer, Integer) -> Integer
middleOfThree (x, y, z)
  |(y >= x && y <= z) || (y >= z && y <= x)= y
  |(x >= y && x <= z) || (x >= z && x <= y)= x
  |otherwise = z

--orderTriple: returns an increasing version of a triple
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = (minOfThree(x, y, z), middleOfThree(x, y, z), maxOfThree(x, y, z))


--swap: function that swaps the first & last elements
swap :: (Char, Char, Char, Char)->(Char, Char, Char, Char)
swap (a, b, c, d) = (d, b, c, a)

--negateTwoDigits: negates any double digit integers
negateTwoDigits :: [Integer] -> [Integer]
negateTwoDigits x = [if k > 9 then -k else k | k <- x]

--matches: function picks out all instances of an integer n from a list
matches :: Integer -> [Integer] -> [Integer]
matches x y = [x | x' <- y, x' == x]

--element: uses matches, returns true is an element is a memeber of a list, otherwise false
element :: Integer -> [Integer] -> Bool
element x y = (matches x y) /= []
