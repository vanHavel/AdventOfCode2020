module Days.Day17 where

import Data.Set (Set)
import qualified Data.Set as Set

run :: String -> String 
run s = let grid = lines s 
            inits = [[i, j] | i <- [0..(length grid - 1)], j <- [0..(length (grid !! 0) - 1)], grid !! i !! j == '#']
            s1 = Set.fromList $ map (0:) inits
            res1 = Set.size $ iterate step s1 !! 6
            s2 = Set.fromList $ map ([0, 0] ++) inits
            res2 = Set.size $ iterate step s2 !! 6
        in show res1 ++ ", " ++ show res2

neighbors :: [Int] -> [[Int]]
neighbors [] = [[]]
neighbors (x:xs) = concatMap (\a -> map (a:) (neighbors xs)) [x - 1, x, x + 1]

step :: Set [Int] -> Set [Int]
step s = let toCheck = Set.fromList $ concatMap neighbors s in Set.filter active toCheck
  where active l = let activeNeighbors = length $ filter (\r -> r /= l && Set.member r s) (neighbors l)
                   in activeNeighbors == 3 || (activeNeighbors == 2 && Set.member l s)

