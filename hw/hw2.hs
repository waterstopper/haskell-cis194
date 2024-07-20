-- https://www.seas.upenn.edu/~cis1940/spring13/hw/02-ADTs.pdf

-- log file parsing
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage msg = case msg of
    'E':' ':rest -> parseErrorMessage rest
    'W':rest -> parseWarningMessage rest
    'I':rest -> Info
    unknown -> Unknown unknown

trimNumber :: Integer -> String -> (Integer, String)
trimNumber inc (x:xs) = case x of
    ' ' -> (inc, xs)
    digit -> trimNumber (inc * 10 + (read [digit] :: Integer)) xs

parseInfoMessage :: String -> LogMessage
parseInfoMessage msg = LogMessage Info (trimNumber 0 msg)


parse :: String -> [LogMessage]
parse [] = []
parse (x:xs) = parseMessage x : parse xs