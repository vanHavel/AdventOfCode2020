module Days.Day3 where 

import Data.List 

run :: String -> String 
run s = 
    let grid  = cycle . transpose . map parse $ lines s
        trees1 = countTrees grid 3 1
        trees2 = product [countTrees grid r d| (r, d) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]
    in show trees1 ++ ", " ++ show trees2

parse :: String -> [Bool]
parse = map (== '#')

countTrees :: [[Bool]] -> Int -> Int -> Int
countTrees grid right down = walk grid 0
  where walk grid i | i >= length (head grid) = 0
                    | otherwise = walk (drop right grid) (i + down) + (if (head grid) !! i then 1 else 0)
