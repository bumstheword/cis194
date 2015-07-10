{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message =
	case words message of
		("I":time:msg) -> LogMessage Info (read time :: Int) (unwords msg)
		("W":time:msg) -> LogMessage Warning (read time :: Int) (unwords msg)
		("E":err:time:msg) -> LogMessage (Error (read err :: Int)) (read time :: Int) (unwords msg)
		msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

msgTimestamp :: LogMessage -> Maybe TimeStamp
msgTimestamp (Unknown _) = Nothing
msgTimestamp (LogMessage _ time _) = Just time

-- Insert a log message into a sorted binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf         = Node Leaf msg Leaf
insert msg (Node left node right)
	| msgTimestamp msg < msgTimestamp node = Node (insert msg left) node right
	| otherwise                            = Node left node (insert msg right)

-- Construct a sorted binary tree from a list of log messages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Return a list of log messages sorted by timestamp
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder msg@(Node left node right)
	| msg == Node Leaf node Leaf = node : []
	| otherwise = inOrder left ++ node : inOrder right

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error n) _ _)
	| n > 50    = True
	| otherwise = False
isSevereError _ = False

getMsg :: LogMessage -> String
getMsg (Unknown str) = str
getMsg (LogMessage _ _ str) = str

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter isSevereError . inOrder . build



alkdjsf
