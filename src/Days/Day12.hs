module Days.Day12 where

import Prelude hiding (Left, Right)

data Command = North Int | South Int | East Int | West Int | Left Int | Right Int | Forward Int deriving (Show, Eq)
fromString :: String -> Command 
fromString (c:n) = let num = read n in case c of 
    {'N' -> North num; 'S' -> South num; 'E' -> East num; 'W' -> West num;
     'L' -> Left (num `div` 90); 'R' -> Right (num `div` 90); 'F' -> Forward num}

data Direction = N | S | E | W deriving (Show, Eq)
turnLeft :: Direction -> Direction 
turnLeft N = W 
turnLeft W = S 
turnLeft S = E 
turnLeft E = N 
turnRight :: Direction -> Direction 
turnRight = turnLeft . turnLeft . turnLeft

type Position = (Int, Int)
move :: Position -> Direction -> Command -> (Position, Direction)
move (y, x) d c = case c of 
    North i -> ((y - i, x), d)
    South i -> ((y + i, x), d)
    East i  -> ((y, x + i), d)
    West i  -> ((y, x - i), d)
    Left i  -> ((y, x), iterate turnLeft d !! i) 
    Right i -> ((y, x), iterate turnRight d !! i)
    Forward i -> case d of 
        N -> ((y - i, x), d)
        S -> ((y + i, x), d)
        E -> ((y, x + i), d)
        W -> ((y, x - i), d)

moveWay :: Position -> Position -> Command -> (Position, Position)
moveWay s@(sy, sx) w@(wy, wx) c = case c of 
    North i -> (s, (wy - i, wx))
    South i -> (s, (wy + i, wx))
    East i  -> (s, (wy, wx + i))
    West i  -> (s, (wy, wx - i))
    Left 0 -> (s, w)
    Right 0 -> (s, w)
    Left i ->  moveWay s (-wx, wy) (Left $ pred i)
    Right i -> moveWay s (wx, -wy) (Right $ pred i)
    Forward i -> ((sy + i * wy, sx + i * wx), w)

run :: String -> String 
run s = let moves = map fromString $ lines s 
            ((y, x), _) = foldl (uncurry move) ((0, 0), E) moves
            ((sy, sx), _) = foldl (uncurry moveWay) ((0, 0), (-1, 10)) moves
            res1 = abs y + abs x
            res2 = abs sy + abs sx
        in show res1 ++ ", " ++ show res2