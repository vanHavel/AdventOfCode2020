module Days.Day25 where 

run :: String -> String 
run s = let subs = map read $ lines s 
            roots = map (getLoop 1 7) subs 
            res = iterate (op $ subs !! 0) 1 !! (roots !! 1)
        in show res

op :: Int -> Int -> Int 
op sub i = (sub * i) `mod` 20201227

getLoop :: Int -> Int -> Int -> Int 
getLoop cur sub goal | cur == goal = 0
                     | otherwise = succ $ getLoop (op sub cur) sub goal