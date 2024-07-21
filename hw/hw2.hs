-- https://www.seas.upenn.edu/~cis1940/spring13/hw/02-ADTs.pdf
-- log file parsing
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg = case msg of
  'E' : ' ' : rest -> parseErrorMessage (trimNumber 0 rest)
  'W' : ' ' : rest -> parseNonErrorMessage Warning rest
  'I' : ' ' : rest -> parseNonErrorMessage Info rest
  unknown -> Unknown unknown

trimNumber :: Int -> String -> (Int, String)
trimNumber inc (x : xs) = case x of
  ' ' -> (inc, xs)
  digit -> trimNumber (inc * 10 + (read [digit] :: Int)) xs

parseNonErrorMessage :: MessageType -> String -> LogMessage
parseNonErrorMessage msgType msg = uncurry (LogMessage msgType) (trimNumber 0 msg)

parseErrorMessage :: (Int, String) -> LogMessage
parseErrorMessage (timestamp, str) = uncurry (LogMessage (Error timestamp)) (trimNumber 0 str)

parse :: String -> [LogMessage]
parse [] = []
parse text = parseLines (lines text)

-- parse (x:xs) = parseMessage x : parse xs

parseLines :: [String] -> [LogMessage]
parseLines = map parseMessage

-- ordering logs
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg tree = case tree of
  Leaf -> Node Leaf msg Leaf
  (Node left someMsg right) ->
    ( if getTimestamp msg > getTimestamp someMsg
        then Node left someMsg (insert msg right)
        else Node (insert msg left) someMsg right
    )

build :: [LogMessage] -> MessageTree
build list = insertIntoTree list Leaf

insertIntoTree :: [LogMessage] -> MessageTree -> MessageTree
insertIntoTree [] tree = tree
insertIntoTree (x : xs) tree = insertIntoTree xs (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
  Leaf -> []
  (Node left msg right) -> inOrder left ++ (msg : inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = map getMessage (filterAndSort list)

filterAndSort :: [LogMessage] -> [LogMessage]
filterAndSort list = inOrder (build (filter isBigErrorMsg list))

isBigErrorMsg :: LogMessage -> Bool
isBigErrorMsg msg = case msg of
    (LogMessage (Error severity) _ _) -> severity >= 50
    _ -> False
