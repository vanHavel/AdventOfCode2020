{-# LANGUAGE BangPatterns #-}

module Days.Day23 where 

import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

moveM :: Int -> Map Int Int -> (Int, Map Int Int) 
moveM cur !m = 
    let {n1 = m Map.! cur; n2 = m Map.! n1; n3 = m Map.! n2; after = m Map.! n3; target = getTarget cur [n1, n2, n3]; afterTarget = m Map.! target}
    in (after, foldr (uncurry Map.insert) m [(cur, after), (target, n1), (n3, afterTarget)])
  where getTarget c ts = head [i | i <- map (\i -> if i < 1 then i + length m else i) [c-1,c-2,c-3,c-4], i `notElem` ts]

pairMap :: [Int] -> Map Int Int 
pairMap xs = Map.fromList $ zip xs (tail xs ++ [head xs])

run :: String -> String 
run s = let firsts = map digitToInt s
            m1 = snd $ iterate (uncurry moveM) (head firsts, pairMap firsts) !! 100
            res1 = map intToDigit $ take 8 $ tail $ iterate (m1 Map.!) 1
            alls = firsts ++ [10..1000000]
            m2 = snd $ iterate (uncurry moveM) (head alls, pairMap alls) !! 10000000
            res2 = m2 Map.! 1 * m2 Map.! (m2 Map.! 1)
        in res1 ++ ", " ++ show res2