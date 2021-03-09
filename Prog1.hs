{- ################################
  Caitlin Harris
  Homework 1
  ################################ -}

module Prog1 where

--isPositive: returns whether a given float is greater than or equal to 0
isPositive :: Float -> Bool
isPositive n 
  |n >= 0 = True
  |otherwise = False

--dividesEvenlyByFive: returns whether the dividend is evenly divisible by 5
dividesEvenlyByFive :: Integer -> Bool
dividesEvenlyByFive n 
  |n `mod` 5 == 0 = True
  |otherwise = False

--middle: returns the 2nd greatest of three int arguments
middle :: Int -> Int -> Int -> Int
middle x y z
  |((x > y) && (x < z)) || ((x < y) && (x > z)) = x
  |((y > x) && (y < z)) || ((y < x) && (y > z)) = y
  |otherwise = z

--nor: computes the NOR gate of two booleans
nor :: Bool -> Bool -> Bool
nor x y
  |((x==y) && (x == False))= True 
  |otherwise = False

--triangleArea: computes the area of a triangle
triangleArea :: Integer -> Integer -> Float
triangleArea x y = (0.5)*(fromInteger x)*(fromInteger y) 

--ceilingDecimal: calculates the ceiling of a float
ceilingDecimal :: Float -> Float
ceilingDecimal n = (fromInteger (ceiling n))

--letterGrade: returns the equivalent letter grade for given int
letterGrade :: Integer -> String
letterGrade n 
  |(n <= 100) && (n >= 93) = "A"
  |(n <= 92) && (n >=90) = "A-"
  |(n <= 89) && (n >= 87) = "B+"
  |(n <= 86) && (n >= 83) = "B"
  |(n <= 82) && (n >= 80) = "B-"
  |(n <= 79) && (n >= 77) = "C+"
  |(n <= 76) && (n >= 73) = "C"
  |(n <= 72) && (n >= 70) = "C-"
  |(n <= 69) && (n >= 67) = "D+"
  |(n <= 66) && (n >= 63) = "D"
  |(n <= 62) && (n >= 60) = "D-"
  |(n < 60) = "F"

--averageThree: returns the average of three integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromInteger(x + y + z)/3

--howManyAboveAverage: returns how many of the three arguments are above it's average
howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z 
  |(fromInteger x > averageThree x y z && fromInteger y > averageThree x y z && fromInteger z > averageThree x y z) = 3
  |(fromInteger x > averageThree x y z) && (fromInteger y > averageThree x y z) = 2  
  |(fromInteger y > averageThree x y z) && (fromInteger z > averageThree x y z) = 2
  |(fromInteger x > averageThree x y z) && (fromInteger z > averageThree x y z) = 2
  |(fromInteger x > averageThree x y z) || (fromInteger y > averageThree x y z) ||(fromInteger z > averageThree x y z) = 1
  |otherwise = 0
  
  
--howManyWithinThreshold: returns how many arguments are within the threshold
howManyWithinThreshold :: Integer -> Integer -> Integer -> Float -> Integer
howManyWithinThreshold x y z t
  |(x > floor(averageThree x y z - t)&& x < ceiling(averageThree x y z + t))&&(y > floor(averageThree x y z - t)&& y < ceiling(averageThree x y z + t)) && (z > floor(averageThree x y z - t)&& z < ceiling(averageThree x y z + t)) = 3
  |(x > floor(averageThree x y z - t)&& x < ceiling(averageThree x y z + t))&&(y > floor(averageThree x y z - t)&& y < ceiling(averageThree x y z + t)) = 2
  |(y > floor(averageThree x y z - t)&& y < ceiling(averageThree x y z + t))&&(z > floor(averageThree x y z - t)&& z < ceiling(averageThree x y z + t)) = 2
  |(x > floor(averageThree x y z - t)&& x < ceiling(averageThree x y z + t))&&(z > floor(averageThree x y z - t)&& z < ceiling(averageThree x y z + t)) = 2
  |(x > floor(averageThree x y z - t)&& x < ceiling(averageThree x y z + t))||(y > floor(averageThree x y z - t)&& y < ceiling(averageThree x y z + t))||(z > floor(averageThree x y z - t)&& z < ceiling(averageThree x y z + t)) = 1
  |otherwise = 0







