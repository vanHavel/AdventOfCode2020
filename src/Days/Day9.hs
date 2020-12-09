module Days.Day9 where

run :: String -> String 
run s = 
    let nums = map read $ lines s 
        res1 = head [xs !! 25 | xs <- suffixes nums, not $ check (take 25 xs) (xs !! 25)]
        (lo, hi) = head [(i, j) | i <- [0..(length nums - 1)], j <- [i+1..(length nums - 1)], sum (take (j - i + 1) $ drop i nums) == res1]
        [mini, maxi] = map ($ take (hi - lo + 1) $ drop lo nums) [minimum, maximum] 
    in show res1 ++ ", " ++ show (mini + maxi)
  where 
      suffixes [] = []
      suffixes (x:xs) = (x:xs) : suffixes xs

check :: [Int] -> Int -> Bool
check xs n = not $ null [(i, j) | i <- xs, j <- xs, i /= j, i + j == n]