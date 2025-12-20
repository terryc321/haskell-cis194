{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- parse int from a string - without IO 
parseInt :: [Char] -> (Int, Bool, [Char])
parseInt [] = (0 , False , [])
parseInt (' ' : xs) = parseInt xs
parseInt (ch : xs) = if ch >= '0' && ch <= '9' then parseInt2 [ch] xs
                     else (0 , False , ch : xs)

parseInt2 :: [Char] -> [Char] -> (Int, Bool, [Char])
parseInt2 acc [] = parseInt3 acc [] 
parseInt2 acc (ch : xs) = if ch >= '0' && ch <= '9' then parseInt2 (ch : acc) xs
                          else parseInt3 acc (ch : xs)

parseInt3 :: Read a => [Char] -> c -> (a, Bool, c)
parseInt3 acc ys = let nums = reverse acc
                       r    = read nums
                   in (r , True , ys )

chomp [] = []
chomp (' ' : xs) = chomp xs
chomp xs = xs


-- exercise 1
parseMessage :: String -> LogMessage
--parseMessage [] = []

-- Error Int
-- E 70 3 Way too many pickles
parseMessage ('E' : rest) = let (n1,True,r) = parseInt rest
                                (n2,True,rs) = parseInt r
                                e1 = n1 :: Int -- coerce Integer to Int ?
                                t2 = n2 :: TimeStamp -- coerce Int to TimeStamp ?
                                rx = chomp rs
                            in LogMessage (Error e1) t2 rx
-- Info
-- I 11 Initiating self-destruct sequence
parseMessage ('I' : rest) = let (n1,True,r) = parseInt rest                                
                                t1 = n1 :: TimeStamp -- coerce Int to TimeStamp ?
                                rx = chomp r
                            in LogMessage Info t1 rx
                               

-- Warning
-- W 5 Flange is due for a check-up
parseMessage ('W' : rest) = let (n1,True,r) = parseInt rest                                
                                t1 = n1 :: TimeStamp -- coerce Int to TimeStamp ?
                                rx = chomp r
                            in LogMessage Warning t1 rx
-- Unknown                                
parseMessage xs = Unknown xs                               

                               
                               
parseMessageTest = [ parseMessage "E 70 3 Way too many pickles" == LogMessage (Error 70) 3 "Way too many pickles" ,
                   parseMessage "I 11 Initiating self-destruct sequence" == LogMessage Info 11 "Initiating self-destruct sequence" ,
                   parseMessage "W 5 Flange is due for a check-up" == LogMessage Warning 5 "Flange is due for a check-up",
                   parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help",
                   parseMessage "I 29 la la la" == LogMessage Info 29 "la la la",
                   parseMessage "This is not in the right format" == Unknown "This is not in the right format"]

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)
-- :browse Prelude
-- testParse parse 10 "error.log"
-- seems ok on parse

-- exercise 2



test :: [(String, [Bool])]
test = [("parseMessage" , parseMessageTest)]

                               

                               


