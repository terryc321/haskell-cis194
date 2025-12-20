{-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE MultilineStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}

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
-- testParse parse 5523 "error.log"
-- seems ok on parse

-- exercise 2
-- A MessageTree should be sorted by timestamp: that is,
-- the times-stamp of a LogMessage in any Node should be greater than all times-stamps
-- of any LogMessage in the left subtree,
-- and less than all times-stamps of any LogMessage in the right child
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf -- only one log message
insert lm2 (Node left lm right) = let (LogMessage _ ts _) = lm
                                      (LogMessage _ ts2 _) = lm2
                                  in if ts2 > ts then Node left lm (insert lm2 right)
                                     else if ts2 == ts then Node left lm2 right
                                          else Node (insert lm2 left) lm right
                                          
-- exercise 3 
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

-- exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l n r) = let il = inOrder l
                           nd = [n]
                           ir = inOrder r
                       in il ++ nd ++ ir
                          
-- inOrder (build tree) ??
-- inOrder (build log)

-- whatWentWrong :: [LogMessage] -> [String]

-- multiline = [r|<HTML>
-- <HEAD>
-- <TITLE>Auto-generated html formated source</TITLE>
-- <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1252">
-- </HEAD>
-- <BODY LINK="800080" BGCOLOR="#ffffff">
-- <P> </P>
-- <PRE>|]

-- multiline = [q|
-- hello line 1
-- hello line 2
-- hello line 3
-- |]

logfile :: [String]
logfile = ["I 6 completed armadillo processing","I 1 Nothing to report",
            "E 99 10 Flange failed!",
            "I 4 Everything normal",
            "I 11 Initiating self-destruct sequence",
            "E 70 3 Way too many pickles",
            "E 65 8 Bad pickle-flange interaction detected",
            "W 5 Flange is due for a check-up",
            "I 7 Out for lunch, back in two time steps",
            "E 20 2 Too many pickles",
            "I 9 Back from lunch"]
          

-- severity error greater than 50 >50
-- :t True
-- :t False
severe :: [LogMessage] -> [String]
severe [] = []
severe ((LogMessage (Error n) _ str) : ys)  = let srest = severe ys
                                               in if n > 50 then str : srest
                                                  else srest
severe (_ : ys) = severe ys

exx5 :: [String]
exx5 = severe $ inOrder $ build $ parse $ (unlines logfile)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = severe . inOrder . build

ex5 :: IO [String]
ex5 = testWhatWentWrong parse whatWentWrong "sample.log"

-- we cannot test an io ??
-- whatWentWrongTest2 = [((testWhatWentWrong parse whatWentWrong "sample.log") == IO ["Way too many pickles","Bad pickle-flange interaction detected","Flange failed!"])]

-- optional exercise 6
-- single egotistical hacker 

                                 


insertTest :: [Bool]
insertTest = [insert (LogMessage (Error 2) 562 "help help") Leaf == Node Leaf (LogMessage (Error 2) 562 "help help") Leaf
             ]


test :: [(String, [Bool])]
test = [("parseMessage" , parseMessageTest),
        ("insert" , insertTest) ]

                               

                               


