module Days.Day10 where 

import Data.List ( sort )

run :: String -> String
run s = let nums = sort $ map read (lines s)
            fixed = 0 : nums ++ [maximum nums + 3]
            diffs = zipWith subtract fixed (tail fixed)
            res1 = length (filter (==1) diffs) * length (filter (==3) diffs)
            res2 = arrangements fixed
        in show res1 ++ ", " ++ show res2

arrangements :: [Int] -> Integer 
arrangements nums = head memo
  where memo = [go i | i <- [0..(length nums - 1)]]
        go i | i == (length nums - 1) = 1
             | otherwise = let (x:xs) = drop i nums 
               in sum [memo !! (i+k) | k <- [1..3], length xs >= k, xs !! (k-1) <= x + 3]