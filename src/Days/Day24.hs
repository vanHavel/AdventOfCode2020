module Days.Day24 where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List

type Position = (Int, Int)
data Direction = NW | NE | E | SE | SW | W deriving (Show, Eq)

move :: Position -> Direction -> Position 
move (y, x) d = case d of 
    NW -> (y-1, x-1)
    NE -> (y-1, x)
    E -> (y, x+1)
    SE -> (y+1, x+1)
    SW -> (y+1, x)
    W -> (y, x-1)

neighbors :: Position -> [Position]
neighbors pos = [move pos dir | dir <- [NW, NE, E, SE, SW, W]]

run :: String -> String 
run s = let dirss = map parse $ lines s 
            grid = foldl flipTile Set.empty dirss
            grids = iterate turn grid
        in show (length grid) ++ ", " ++ show (length $ grids !! 100)

flipTile :: Set Position -> [Direction] -> Set Position
flipTile s dirs = 
    let target = foldl move (0, 0) dirs 
    in if Set.member target s then Set.delete target s else Set.insert target s

turn :: Set Position -> Set Position 
turn s = let toCheck = nub $ Set.toList s ++ concatMap neighbors (Set.toList s)
         in Set.fromList [pos | pos <- toCheck, active pos s]
  where active pos s = let activeNeighbors = length [p | p <- neighbors pos, Set.member p s]
                       in if Set.member pos s then activeNeighbors `elem` [1,2] else activeNeighbors == 2

parse :: String -> [Direction]
parse [] = []
parse ('n':'w':xs) = NW : parse xs 
parse ('n':'e':xs) = NE : parse xs 
parse ('s':'w':xs) = SW : parse xs 
parse ('s':'e':xs) = SE : parse xs 
parse ('w':xs) = W : parse xs 
parse ('e':xs) = E : parse xs 
