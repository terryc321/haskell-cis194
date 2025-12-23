
module Higher where

ho = "Ho!"

-- exercise 1 
-- Wholemeal programming 
-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
-- | even x = (x - 2) * fun1 xs
-- | otherwise = fun1 xs
-- 2. fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n = n + fun2 (n ‘div‘ 2)
-- | otherwise = fun2 (3 * n + 1)

-- iterate
-- iterate creates an infinite list
-- :t iterate
-- iterate :: (a -> a) -> a -> [a]
--           procedure 1 arg  -> initial value  -> infinite list
-- take 5 (iterate (+ 1) 1)
-- [1,2,3,4,5]
-- ghci> take 5 (iterate (+ 2) 1)
-- [1,3,5,7,9]

-- takeWhile
-- :t takeWhile
-- takeWhile :: (a -> Bool) -> [a] -> [a]
--              predicate on type a -> list of type a  -> a list pass the predicate
--            stop if meet element does not pass predicate
-- takeWhile even [2,4,6,8,11,2,4]
-- [2,4,6,8]
-- collect all even numbers until meet either end of list or odd number

-- not really sure purpose of this exercise , except use other ideas reach result
-- less efficient as iterating over a generated list several times
-- perhaps optimised away 
fun1 :: [Integer] -> Integer
fun1 xs = foldr (*) 1 $ map (\x -> x - 2) $ filter even xs

testEx1 = [ fun1 [4] == 2 ,fun1 [4,4] == 4 ,fun1 [4,4,4] == 8 ]
-- testEx1 should all be True True True ...

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- similar idea to collatz , probably a world away
collatz = fun2


-- still have no idea how do i do something with an IO String if have pure functions
-- that use String ?

-- exercise 2 
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)

-- foldTree generates a balanced binary tree from a list of values using foldr
-- foldTree "ABCDEFGHIJ" == ...
-- ABCDE . FGHIJ
-- 12345   12345
-- split string into two equal halfs ? how ? why ? what for ?
-- how make a balanced binary tree ?


foldTree :: [a] -> Tree a
foldTree [] = Leaf
--?? no idea
