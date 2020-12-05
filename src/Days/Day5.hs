module Days.Day5 where

import Data.List

run :: String -> String 
run s =
    let vals = map parse $ lines s
        res1 = maximum vals 
        res2 = seat (sort vals)
    in show res1 ++ ", " ++ show res2

parse :: String -> Int 
parse = (\[a, b, c, d, e, f, g, h, i, j] -> (64*a+32*b+16*c+8*d+4*e+2*f+g)*8+4*h+2*i+j) . map (\c -> if c `elem` "BR" then 1 else 0)

seat :: [Int] -> Int
seat (x:y:xs) | y /= succ x = succ x 
              | otherwise   = seat (y:xs)