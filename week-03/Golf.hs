

module Golf where

-- skips :: [a] -> [[a]]
-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the nth list in
-- the output should contain every nth element from the input list.
-- For example:
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
-- Note that the output should be the same length as the input.

-- skips [] = []
-- skips (x : []) = [x]

-- skips xs = let len = length xs
--            in skips2 1 len xs

-- skips2 n m xs = if n > m then []
--                 else let a = skippy n xs
--                          b = skips2 (n + 1) m xs
--                      in a : b

-- exercise 1
skips :: [a] -> [[a]]
skips [] = []
skips ys = let len = length ys
           in map (\m -> skippy 1 m ys) [ 1 .. len]


skippy :: Int -> Int -> [a] -> [a]
skippy n m [] = []
skippy n m (x : xs) = if n == m then x : (skippy 1 m xs)
                      else skippy (n + 1) m xs

-- -- exercise 2
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4, since
-- it is greater than the elements immediately before and after it (3 and
-- 1). 5 is not a local maximum since there is no element that comes
-- after it.
-- Write a function
-- localMaxima :: [Integer] -> [Integer]
-- which finds all the local maxima in the input list and returns them in
-- order. For example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

-- inner xs = butfirst $ butlast xs

-- butfirst [] = []
-- butfirst (x:xs) = xs

-- butlast xs = reverse $ butfirst $ reverse xs

-- localMaxima [] = []
-- localMaxima (x : []) = []
-- localMaxima (x : y : []) = []
localMaxima (x : y : z : zs) = let loc = localMaxima (y : z : zs)
                               in  if (y > x && y > z)  then y : loc  else loc 
localMaxima _ = []


-- exercise 3 histogram
histogram :: [Integer] -> String
histogram xs = let ns = map (\x -> (x,0)) [0 .. 9]
               in histo2 xs ns

hmax xs = histomax xs 0
histomax [] n = n
histomax ((a,b) : zs) n = histomax zs m
                          where m = max b n

incr :: Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
incr x [] = []
incr x ((a,b) : xs) = if x == a then (x,b+1) : xs
                      else (a,b) : (incr x xs)

histo2 :: [Integer] -> [(Integer,Integer)] -> String
histo2 [] ns = graph ns (hmax ns)
histo2 (x:xs) ns = histo2 xs (incr x ns)

graph :: [(Integer,Integer)] -> Integer -> String
graph ns 0 = "==========\n0123456789\n"
graph ns m = (agraph ns m) ++ "\n" ++ (graph ns (m - 1))

-- make a string from (0,1)
agraph :: [(Integer,Integer)] -> Integer -> String
agraph [] m = []
agraph ((a,b):zs) m = let ags = agraph zs m
                      in if b >= m then '*' : ags
                         else ' ' : ags

                                


-- tests 
testSkips :: [Bool]
testSkips = [ let a :: [Int] = [] in skips a == [] ,
              skips [1] == [[1]] ,
              skips [True,False] == [[True,False],[False]],
              skips "hello!"==["hello!","el!","l!","l","o","!"],
              skips "ABCD" == ["ABCD","BD","C","D"]]

testMaxima :: [Bool]
testMaxima = [ localMaxima [2,9,5,6,1] == [9,6] ,
               localMaxima [2,3,4,1,5] == [4],
               localMaxima [1,2,3,4,5] == [] ]

testHisto :: [Bool]
testHisto = [histogram [3,5] == "   * *    \n==========\n0123456789\n",
  histogram [1,4,5,4,6,6,3,4,2,4,9] == "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n",
  histogram [1,1,1,5] == " *        \n *        \n *   *    \n==========\n0123456789\n"]


test:: [(String,[Bool])]
test = [("skips", testSkips),
        ("maxima", testMaxima),
        ("histo", testHisto)]                           


-- putStr $ histogram [1,1,1,5]
--  *        
--  *        
--  *   *    
-- ==========
-- 0123456789       

-- putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
--     *     
--     *     
--     * *   
--  ******  *
-- ==========
-- 0123456789

