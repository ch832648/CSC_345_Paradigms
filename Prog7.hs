{- ######################
   Caitlin Harris
   Homework 7.
   ###################### -}

module Prog7 where

data Expr = Val Int
          |Add Expr Expr
          |Sub Expr Expr
          |Mul Expr Expr
          |Div Expr Expr

--1. eval: evaluates an expression
--Add catch for dividing by zero?
eval :: Expr -> Int
eval (Val n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) =  div (eval e1)(eval e2)

--2. maxLit: returns the greatest interger literal in an expresssion
maxlit :: Expr -> Int
maxlit (Val x) = x
maxlit (Add e1 e2) = maximum(maxlit e1 : maxlit e2 : [])
maxlit (Sub e1 e2) = maximum(maxlit e1 : maxlit e2 : [])
maxlit (Mul e1 e2) = maximum(maxlit e1 : maxlit e2 : [])
maxlit (Div e1 e2) = maximum(maxlit e1 : maxlit e2 : [])


--3. safeeval: evaluates an expression and catches any division errors
safeeval :: Expr -> Maybe Int
safeeval (Val n) = Just n
safeeval (Div e1 (Val 0)) = Nothing

--show :: Expr -> String

--4. Instance of the Show class 
e1, e2, e3 :: Expr
e1 = Val 5
e2 = Add (Val 3) (Val 2)
e3 = Add (Val 3) (Mul (Val 2) (Val 4))

instance Show Expr where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ "-" ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ "*" ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ "/" ++ show y ++ ")"

--5. addone: takes an expression & returns a new tree 1 to every literal
addone :: Expr -> Expr
addone (Val x) = Val (x+1)
addone (Add e1 e2) = Add(addone e1) (addone e2)
addone (Sub e1 e2) = Sub(addone e1) (addone e2)
addone (Mul e1 e2) = Mul(addone e1) (addone e2)
addone (Div e1 e2) = Div(addone e1) (addone e2)

--6. containing: returns whether each element in 1st list is in the 2nd
containing :: Eq a => [a] -> [a] -> Bool
containing x [] = False
containing [] y = True
containing (x:xs) y
  |elem x y = containing xs y
  |otherwise = False

--7. sumSqNeg: computes the "sum of squares of negatives"
sumSqNeg ::  [Int] -> Int
sumSqNeg xs = foldr (+) 0 (map sqr (filter neg xs))
            where 
            sqr x = x * x
            neg x = x < 0

--8. lengths: returns a list of lengths of given strings
lengths :: [String] -> [Int]
lengths [] = []
lengths xs = map length xs

--9. total: applies the function to the list and sums the result
total :: (Int -> Int) -> [Int] -> Int
total _ [] = 0
total f xs = sum(map f xs) 

--10. containing: returns whether each element in the list is also in the second list
containing' :: Eq a => [a] -> [a] -> Bool
containing' _ [] = False
containing' xs ys = foldr (&&) True (map elem' xs)
  where
    elem' z
      |elem z ys = True
      |otherwise = False    




