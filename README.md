# haskell learning directory

# the special IO monad

the io monad is a special side-effect case where there is no true definition for bind or return
in terms of haskell, simply not a function just a side effect.

also there is no way to get the value out of the IO monad it is simply stuck in IO land unless use
unsafe procedure which is discouraged

# side effects

haskell is lazy which can be problematic for side effectful programs.

# return 

return lifts a value into a monad 

```
(return) :: Monad m => a -> m a
```

# dollar $ 

the dollar symbol means we do not need to surround whatever follows with parentheses.  template haskell has `$(` so be careful to save confusion

```
($) :: (a -> b) -> a -> b

--- can also map 
map ($ 1) [(+ 1),(+ 2), (+ 3)]
```

# GHC , Base and Prelude

"ghc 9.12.1 --- base 4.12.0.0 -- "

for entire hackage of 4.12.0.0 we have

https://hackage.haskell.org/package/base-4.21.0.0/docs/

Prelude is found at 

https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html

Base itself is 

https://hackage.haskell.org/package/base-4.21.0.0/docs/Base.html


# IO monad

io-monad directory has learning we did to teach ourselves how to do something useful like 
read contents of a file

we can read entire file by 
```
readFile "../README.md"
```

cannot read length of String returned even though it says it is a string

```
:set +t  --- enable type explanation of results
(readFile "../README.md") 
it :: String

λ> 3 :: Int
3
it :: Int
λ> 323423423423423482837894798278934789789278934789789278934234 :: Int
<interactive>:30:1-60: warning: [GHC-97441] [-Woverflowed-literals]
    Literal 323423423423423482837894798278934789789278934789789278934234 is out of the Int range -9223372036854775808..9223372036854775807

-8811412722259716902
it :: Int
λ> 


-- read contents of a file
size :: IO Int
size = do contents <- (readFile "../README.md") 
          return (length contents)


-- we can use $ dollar to mean put parens around anything that comes after on same line ?
size :: IO Int
size = do contents <- (readFile "../README.md") 
          return $ length contents


--- inside a do statement
--- we can also add a spurious Int to length of the file 
--- but the result must always be wrapped in an IO monad -- called return 
size :: IO Int
size = do contents <- (readFile "../README.md") 
          return ((length contents) + 123)


--- io pushed to the edges of application ok , 
--- return lifts a value into a monadic value

retS :: IO String
retS = do 
       return "hello"

λ> retS
"hello"
it :: String
--- incorrect actually IO String


-- we can get length of string using fmap -- whatever that is 
fmap length retS 
=> 5
--- again this is an IO Int 


Maybe Monad ??
Just x 
Nothing 


State Monad ??


Either Monad ?? 




```



# haskell-cis194

```haskell
hello_world = "Hello World"
```

## week-01

validate credit card

## week-02

`LogAnalysis` log parsing

## week-03

`Golf` skips localMaxima histogram

## week-04

higher order programming 

## week-05

got totally lost `...`

typeclasses similar to overloading 

Parser.hs using totally foreign constructs `<$>` `<|>` `<*>`

what is Control.Applicative 

what is Control.Arrow

what is Data.Monoid

what is Data.Char 

what is Data.List 

## week-06

## week-07

## week-08

