
-- IO Monad experiments
--
-- load this file into ghci using :l load command
-- :l GetChar
-- subsequent reloads use :r reload command
-- :r 

module GetChar where

-- main :: IO ()
-- getc :: IO Char
-- gets :: IO String

-- True
-- False

getc :: IO Char
getc = do c <- getChar
          putChar c
          return c 

-- so emacs is lacking haskell usability - indentation of code does not work
-- just so dumb given 2026

-- can main be IO Bool ?
main :: IO Bool
main =  do c <- getc
           putChar c                              
           putChar '\n'
           return (c == 'a')
                              

                              

                              




