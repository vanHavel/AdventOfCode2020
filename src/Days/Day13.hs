module Days.Day13 where 

import Data.List.Split (splitOn)
import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromJust)

run :: String -> String 
run s = let (time, busses) = parse s 
            best = minimumBy (compare `on` snd) [(fromJust bus, leave time (fromJust bus))| bus <- busses, bus /= Nothing]
            congs = [(m, (-i + m*i) `mod` m) | (m, i) <- map (\(a, b) -> (fromJust a, b)) $ filter (\x -> fst x /= Nothing) $ zip busses [0..]]
            res1 = fst best * snd best
        in show res1 ++ ", " ++ show congs --solve congruences using CRT

parse :: String -> (Int, [Maybe Int])
parse s = let [first, second] = lines s
              busses = map (\b -> if b == "x" then Nothing else Just (read b)) $ splitOn "," second
          in (read first, busses)

leave :: Int -> Int -> Int 
leave start period = period - (start `mod` period)