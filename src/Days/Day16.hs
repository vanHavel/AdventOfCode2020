module Days.Day16 where

import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Debug.Trace (traceShow)
run :: String -> String 
run s = let ls = lines s 
            conds = map parseCondition (take 20 ls)
            yours = parseTicket (ls !! 22)
            others = map parseTicket (drop 25 ls)
            res1 = sum [v | t <- others, v <- t, not $ any ($v) conds]
            valid = filter (all (\v -> any ($v) conds)) others
            cands = [[i | i <- [0..19], all (\t -> cond (t !! i)) valid] | cond <- conds]
            sol = concat $ solve cands
            res2 = product [yours !! i | i <- take 6 sol]
        in show res1 ++ ", " ++ show res2

solve ::[[Int]] -> [[Int]]
solve xs | all isSingleton xs = xs 
         | otherwise = let singletons = concat $ filter isSingleton xs 
                       in solve $ map (\l -> if isSingleton l then l else filter (`notElem` singletons) l) xs
  where isSingleton = \l -> length l == 1


parseTicket :: String -> [Int]
parseTicket = map read . splitOn ","

parseCondition :: String -> (Int -> Bool)
parseCondition s = let [a, b, c, d] = map read $ words $ map (\c -> if isDigit c then c else ' ') s
                   in \x -> (x >= a && x <= b) || (x >= c && x <= d)