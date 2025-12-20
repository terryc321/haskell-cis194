-- homework1.lhs

findex [] n acc = acc
findex (x : ys) n acc = findex ys (n+1) ((x,n) : acc)

fdouble [] = []
fdouble (x : ys) = let (a,b) = x
                   in let r = if odd b then a else 2* a
                          in r : (fdouble ys)

doubler xs = fdouble (findex (reverse xs) 1 [])
doublerTest = [doubler [10,20,30,40,50] == [10,40,30,80,50]]

fadder xs = sum (doubler xs)

foo :: Integer -> [Integer]
foo 0 = []
foo n = if n < 0 then []
        else let r = rem n 10
                 d = div n 10
             in r : (foo d)
                

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = foo n

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsTest :: [Bool]
toDigitsTest = [ toDigits 1234 == [1,2,3,4],
                 toDigitsRev 1234 == [4,3,2,1],
                 toDigits 0 == [],
                 toDigits (-17) == []
               ]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = doubler

doubleEveryOtherTest :: [Bool]
doubleEveryOtherTest = [doubleEveryOther [1,2,3] == [1,4,3] ,
                         doubleEveryOther [8,7,6,5] == [16,7,12,5]]
                       
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map sum (map toDigits xs))

sumDigitsTest :: [Bool]
sumDigitsTest = [ sumDigits [16,7,12,5] == 22 ]

validate :: Integer -> Bool
validate n = let xs = toDigits n
                 ys = doubleEveryOther xs              
             in let r = rem (sumDigits ys) 10
                in r == 0
                   

validateTest :: [Bool]
validateTest = [ validate 4012888888881881 == True ,
                 validate 4012888888881882 == False
               ]


runTest = [("toDigits" , toDigitsTest),
           ("doubler", doublerTest ),
           ("doubleEveryOther", doubleEveryOtherTest ),
           ("sumDigits" , sumDigitsTest),
           ("validate" , validateTest)]

-- exercise 5 tower of hanoi
-- exercise 6 optional extended hanoi

















