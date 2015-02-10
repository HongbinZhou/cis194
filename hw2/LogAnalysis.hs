{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Ex1
parseMessage :: String -> LogMessage
parseMessage ('E':xs) = LogMessage (Error errorLevel) timeStamp message
                      where errorLevel = read . head . words $ xs
                            timeStamp  = read . head . drop 1 . words $ xs
                            message  = unwords . drop 2 . words $ xs
parseMessage (x:xs)
             | x == 'W' = LogMessage Warning timeStamp message
             | x == 'I' = LogMessage Info timeStamp message
               where timeStamp  = read . head . words $ xs
                     message  = unwords . drop 1 . words $ xs

parseMessage _ = Unknown "Unknow type message!" 

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

-- Ex2, not test yet!!
getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ timeStamp _) = timeStamp


getLeftTree :: MessageTree -> MessageTree
getLeftTree (Node leftTree _ _ ) = leftTree

getRightTree :: MessageTree -> MessageTree
getRightTree (Node _ _ rightTree) = rightTree

getLogMessage :: MessageTree -> LogMessage
getLogMessage (Node _ logMessage _) = logMessage

-- Give all elements in original tree a mark: '
insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage msgTree'
       | msgTree' == Leaf = Node Leaf logMessage Leaf
       | msgTimeStamp < msgTimeStamp' = Node (insert logMessage leftTree') logMessage' rightTree'
       | msgTimeStamp > msgTimeStamp' = Node leftTree' logMessage' (insert logMessage rightTree')
       | otherwise  = error "the given logMessage's timestamp exist!"
       where leftTree' = getLeftTree msgTree'
             rightTree' = getRightTree msgTree'
             logMessage' = getLogMessage msgTree'
             msgTimeStamp = getTimeStamp logMessage
             msgTimeStamp' = getTimeStamp logMessage'

-- Ex3
build :: [LogMessage] -> MessageTree
build = foldr (\x acc -> insert x acc) Leaf

-- Ex4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree)
        = (inOrder leftTree) ++ [logMessage] ++ (inOrder rightTree)

-- Ex5
isErrorMsg :: LogMessage -> Bool
isErrorMsg (LogMessage (Error _) _ _) = True
isErrorMsg _ = False

-- whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . 
              filter (\(LogMessage (Error x) _ _) -> if x >= 50 then True else False ) . 
              filter isErrorMsg . 
              inOrder . 
              build
              where getMsg (LogMessage (Error _) _ msg) = msg


-- test
logMsgs = [parseMessage "I 562 this is the first info message!"
          ,parseMessage "W 001 this is the first warning message!"
          ,parseMessage "E 2 312 this is the first error message!"]
infoMsg1 = parseMessage "I 562 this is the first info message!"
warnMsg1 = parseMessage "W 001 this is the first warning message!"
errMsg1 = parseMessage "E 2 312 this is the first error message!"

t1 = insert infoMsg1 Leaf
t2 = insert warnMsg1 t1
t3 = insert errMsg1 t2

main = testWhatWentWrong parse whatWentWrong "sample.log"
