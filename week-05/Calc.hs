
module Calc where

import ExprT
import Parser

{-
data ExprT =
  Lit Integer
| Add ExprT ExprT
| Mul ExprT ExprT
deriving (Show, Eq)
-}

-- exercise 1 
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = let ex = eval x
                     ey = eval y
                 in ex + ey
eval (Mul x y) = let ex = eval x
                     ey = eval y
                 in ex * ey                  

--eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

-- exercise 2
-- parseExp Lit Add Mul "(2+3)*4"

-- evalStr :: String -> Maybe ExprT
-- evalStr s = parseExp Lit Add Mul s

-- convert ExprT to Integer use eval
evalStr :: String -> Maybe Integer
evalStr s = let out = parseExp Lit Add Mul s
            in case out of
                 Just a -> Just (eval a)
                 Nothing -> Nothing

-- exercise 3
{-
mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
== Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-}

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


instance Expr ExprT where
  lit n = Lit n
  add = Add
  mul = Mul 

  
reify :: ExprT -> ExprT
reify = id

{-
 :t mul (add (lit 2) (lit 3)) (lit 4)
> mul (add (lit 2) (lit 3)) (lit 4) :: Expr a => a

mul (add (lit 2) (lit 3)) (lit 4) == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
> True

reify $ mul (add (lit 2) (lit 3)) (lit 4)

-}


instance Expr Integer where
  lit n = n
  add x y = x + y
  mul x y = x * y


reifyInt :: Integer -> Integer
reifyInt = id

{-
Bool — every literal value less than or equal to 0 is interpreted as False, and all positive Integers
are interpreted as True; “addition” is logical or,
“multiplication” is logical and
-}

instance Expr Bool where
  lit n = if n < 1 then False else True
  add x y = x && y
  mul x y = x || y

reifyBool :: Bool -> Bool
reifyBool = id


{-
reify $ add (lit 2) (mul (lit 3) (lit 4))
>Add (Lit 2) (Mul (Lit 3) (Lit 4))
it :: ExprT

reifyBool $ add (lit 2) (mul (lit 3) (lit 4))
> True
it :: Bool

reifyInt $ add (lit 2) (mul (lit 3) (lit 4))
>14
it :: Integer

-}

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 n
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

reifyMod7 :: Mod7 -> Mod7
reifyMod7 = id

{-
reifyMod7 $ add (lit 2) (mul (lit 3) (lit 4))
-}

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
-- > -7
testInteger = testExp :: Maybe Integer
-- > -7
testBool = testExp :: Maybe Bool
-- > (True || False ) && True
testMM = testExp :: Maybe MinMax
-- > max (min 3 ,-4) 5 
testSat = testExp :: Maybe Mod7
-- >




