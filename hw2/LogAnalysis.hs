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

insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage msgTree
       | msgTree == Leaf = Node Leaf logMessage Leaf
       | getTimeStamp logMessage < getTimeStamp logMessage' = Node (insert logMessage leftTree) logMessage rightTree
       | getTimeStamp logMessage > getTimeStamp logMessage' = Node leftTree logMessage (insert logMessage rightTree)
       | otherwise  = error "error happened!"
       where leftTree = getLeftTree msgTree
             rightTree = getRightTree msgTree
             logMessage' = getLogMessage msgTree
