module Days.Day1 where

import Data.List

run :: String -> String
run s = 
    let nums = map read $ lines s 
        Just (a, b) = twoSum nums 2020
        Just (c, d, e) = threeSum nums 2020
    in show (a * b) ++ ", " ++ show (c * d * e)

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum nums goal = walk sorted (reverse sorted)
  where sorted = sort nums 
        walk [] _ = Nothing 
        walk _ [] = Nothing
        walk (x:xs) (y:ys) | x + y < goal = walk xs (y:ys)
                           | x + y > goal = walk (x:xs) ys 
                           | otherwise    = Just (x, y)

threeSum :: [Int] -> Int -> Maybe (Int, Int, Int)
threeSum nums goal = firstValid [(x, twoSum (delete x nums) (goal - x)) | x <- nums]
  where firstValid [] = Nothing 
        firstValid ((_, Nothing):xs) = firstValid xs 
        firstValid ((x, Just (y, z)):_) = Just (x, y, z)
