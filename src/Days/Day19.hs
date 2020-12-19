module Days.Day19 where

import Data.Char
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map
import Data.List
data Rule = T Int Char | N Int Int Int deriving (Show)
symbol :: Rule -> Int 
symbol (T i _) = i 
symbol (N i _ _) = i
isT :: Rule -> Bool 
isT (T _ _) = True 
isT _ = False

run :: String -> String 
run s = 
    let rules = concatMap parseRules $ takeWhile (isDigit . head) $ lines s
        words = dropWhile (isDigit . head) $ lines s
        valids = filter (cyk rules) words
    in show (length valids)

cyk :: [Rule] -> String -> Bool 
cyk rules word = dp Map.! (n, 0, 0)
   where n = length word 
         dp = Map.fromList [((l, s, a), go l s a) | l <- [1..n], s <- [0..n-l], a <- nub $ map symbol rules]
         go 1 s a = any (\(T i c) -> i == a && word !! s == c) $ filter isT rules
         go l s a = any (\(N i b c) -> i == a && any (\p -> dp Map.! (p, s, b) && dp Map.! (l-p, s+p, c)) [1..l-1]) $ filter (not . isT) rules

parseRules :: String -> [Rule]
parseRules s = if elem '"' s then [parseT s] else parseN s
  where parseT s = let (num, [char]) = span isDigit (filter (\c -> isDigit c || elem c "ab") s) in T (read num) char
        parseN s = let (num, ':':rest) = span isDigit s 
                       rules = splitOn "|" rest
                   in [N (read num) a b | rule <- rules, let [a, b] = map read $ words rule]