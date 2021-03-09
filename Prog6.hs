
{- ##################
   Caitin Harris
   Homework 6
   ################## -}

module Prog6 where

data Set = Set [Int]
         | EmptySet
  deriving Show

-- 1.equal: returns whether two sets are equal
equal :: Set -> Set -> Bool
equal (EmptySet) (EmptySet) = True
equal _ (EmptySet) = False
equal (EmptySet) _ = False
equal (Set[]) (Set[]) = True
equal (Set (x:xs)) (Set (y:ys))
  |x == y = True
  |otherwise = False


-- 2.saferemove: removes the given item from a set
saferemove :: Int -> Set -> Maybe Set
saferemove _ EmptySet = Nothing
saferemove _ (Set[]) = Nothing
saferemove n (Set ys)
  |elem n ys == False = Nothing
  |otherwise = Just(Set([x | x <- ys, x /= n]))


-- 3.union: takes two sets and returns the union of both sets
helper :: Int -> Set -> Bool
helper _ EmptySet = False
helper _ (Set[]) = False
helper n (Set(y:ys))
  |n == y = True
  |otherwise = helper n (Set ys)

union :: Set -> Set -> Set
union (EmptySet) (EmptySet) = EmptySet
union (EmptySet) (Set ys) = (Set ys)
union (Set xs) (EmptySet) = (Set xs)
union (Set xs) (Set ys) = (Set(xs++[y|y<-ys, helper y (Set xs) == False]))


--data type Tree:
data Tree = Leaf Int 
          | Node Tree Int Tree


--4. preorder: takes a tree argument and returns as a list
--preorder transversal of the tree
preorder :: Tree -> [Int]
preorder (Leaf y) = [y]
preorder (Node left root right) = [root] ++ (preorder left) ++ (preorder right)

--5. postorder: takes a tree argument and returns as a list
-- postorder transversal of the tree
postorder :: Tree -> [Int]
postorder (Leaf y) = [y]
postorder (Node left root right) = (preorder left) ++ (preorder right) ++ [root]

--6.countZeros: takes a tree arg and returns an Int
--the number of zeros in the tree
countZeros :: Tree -> Int
countZeros xs = length[x | x <- (preorder xs), x == 0]

--7. countLeaves: returns the number of leaves in the given tree
countLeaves :: Tree -> Int
countLeaves (Leaf y) = 1
countLeaves (Node left root right) = countLeaves left + countLeaves right

--8. countInteriorNodes: returns the number of interior nodes
countInteriorNodes (Leaf y) = 0
countInteriorNodes (Node left root right) = 1 + (countInteriorNodes left) + (countInteriorNodes right)


--9. depth:: returns the depth of a tree
depth :: Tree -> Int
depth (Leaf _) = 0
depth (Node left root right) = 1 + (max (depth left) (depth right))


--10. balanced:: returns if the tree is balanced
helper2 :: Tree -> Int
helper2 (Leaf y) = 1
helper2 (Node left root right) = helper2 left + helper2 right

balanced :: Tree -> Bool
balanced (Leaf y) = True
balanced (Node left root right) = (bal <= 1) && (bal >= (-1))
  where bal = (helper2 left) - (helper2 right)





