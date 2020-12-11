module Days.Day11 where

import Data.Array

type Grid = Array (Int, Int) Char

run :: String -> String
run s = 
    let grid = parse s 
        stati1 = iterate step1 grid 
        res1 = firstSame stati1
        stati2 = iterate step2 grid 
        res2 = firstSame stati2
    in show res1 ++ ", " ++ show res2

parse :: String -> Grid
parse s = 
    let width = length . head $ lines s 
        height = length $ lines s
        bounds = ((1, 1), (height, width))
    in listArray bounds (concat $ lines s)

step1 :: Grid -> Grid
step1 arr = array (bounds arr) [(i, update i e) | (i, e) <- assocs arr]
  where update (y, x) c = let surroundings = [arr ! (y + a, x + b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0, inbounds (y + a, x + b)]
                              in case c of 
                                  '.' -> '.'
                                  'L' -> if not ('#' `elem` surroundings) then '#' else 'L' 
                                  '#' -> if length (filter (== '#') surroundings) >= 4 then 'L' else '#'
        inbounds (y, x) = let (my, mx) = snd (bounds arr) in y > 0 && x > 0 && y <= my && x <= mx

step2 :: Grid -> Grid
step2 arr = array (bounds arr) [(i, update i e) | (i, e) <- assocs arr]
  where update (y, x) c = let surroundings = [walk (y + a, x + b) (a, b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0]
                              in case c of 
                                  '.' -> '.'
                                  'L' -> if not ('#' `elem` surroundings) then '#' else 'L' 
                                  '#' -> if length (filter (== '#') surroundings) >= 5 then 'L' else '#'
        inbounds (y, x) = let (my, mx) = snd (bounds arr) in y > 0 && x > 0 && y <= my && x <= mx
        walk (y, x) (a, b) | not (inbounds (y, x)) = '.' 
                           | otherwise = case arr ! (y, x) of 
                               '.' -> walk (y + a, x + b) (a, b)
                               c -> c


firstSame :: [Grid] -> Int 
firstSame (x:y:xs) | x == y = length (filter (== '#') $ elems x)
                   | otherwise = firstSame (y:xs)