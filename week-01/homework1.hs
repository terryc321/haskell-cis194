-- homework1.lhs

-- double the value of every second digit beginning from the right.
-- That is, the last digit is unchanged; the second-to-last digit is dou-
-- bled; the third-to-last digit is unchanged; and so on. For example,
-- [1,3,8,6] becomes [2,3,16,6]
-- double [] = []
-- double (x : []) = [2 * x]
-- double (y : x : []) = y : (2 * x) : []
-- double (y : x : z) = y : (2*x) : (double z)
-- reverse ... reverse xs
--doubleEveryOther [] = []
-- doubleEveryOther xs = doubleEveryOther2 (reverse xs)

-- give each element an index
-- findex :: (Num b, Num a) => [(a, b)]
findex [] n acc = acc
findex (x : ys) n acc = findex ys (n+1) ((x,n) : acc)

fdouble [] = []
fdouble (x : ys) = let (a,b) = x
                   in let r = if odd b then a else 2* a
                          in r : (fdouble ys)


doubler xs = fdouble (findex (reverse xs) 1 [])
-- doubler [10,20,30,40,50]
-- [10,40,30,80,50]

fadder xs = sum (doubler xs)

-- repeatedly peel off until zero remainder?

foo 0 = []
foo n = let r = rem n 10
            d = div n 10
        in r : (foo d)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = foo n

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)















