module Days.Day21 where 

import Data.List.Split
import qualified Data.Map as Map
import Data.List
import Data.Map (Map)
run :: String -> String 
run s = 
    let samples = map parse $ lines s 
        ingredients = nub $ concatMap fst samples
        allergens = nub $ concatMap snd samples
        candidates = Map.fromList [(a, filter (\i -> all (elem i) [is | (is, as) <- samples, elem a as]) ingredients) | a <- allergens]
        res1 = length [i | is <- samples, i <- fst is, notElem i $ concat $ Map.elems candidates]
        res2 = intercalate "," $ map snd $ sort $ Map.assocs $ solve candidates
    in show res1 ++ ", " ++ show res2

solve :: Map String [String] -> Map String String 
solve m | all isSingle (Map.elems m) = Map.map (\[a] -> a) m
        | otherwise = solve $ Map.map (\l -> if isSingle l then l else filter (\a -> notElem a $ concat [r | r <- Map.elems m, isSingle r]) l) m
  where isSingle l = length l == 1

parse :: String -> ([String], [String])
parse line = 
    let (begin, end) = span (/= '(') line 
    in (words begin, splitOn ", " $ filter (/= ')') $ drop 10 end)