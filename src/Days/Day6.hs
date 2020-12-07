module Days.Day6 where

import Data.List ( nub, intersect )
import Data.List.Split ( splitOn )

run :: String -> String
run input = 
    let groups = map (splitOn "\n") $ splitOn "\n\n" input
        res1 = sum . map (length . nub . concat) $ groups
        res2 = sum . map (length . foldr1 intersect) $ groups
    in show res1 ++ ", " ++ show res2