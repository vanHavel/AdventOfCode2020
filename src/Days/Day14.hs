module Days.Day14 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 
import Data.Char (digitToInt, isDigit)
import Data.Bits

type Memory = Map Int Int 
data Command = Mask Int Int Int | Set Int Int deriving (Eq, Show)
data ProgramState = ProgramState {mask :: (Int, Int, Int), memory :: Memory}


run :: String -> String 
run s = 
    let cmds = map parse $ lines s 
        mem = runProgram ProgramState{memory=Map.empty, mask=(0, 0, 0)} cmds
        float = runFloater ProgramState{memory=Map.empty, mask=(0, 0, 0)} cmds
    in show (sum $ Map.elems mem) ++ ", " ++ show (sum $ Map.elems float)

runProgram :: ProgramState -> [Command] -> Memory 
runProgram ps [] = memory ps 
runProgram ps (c:cs) = case c of 
    Mask a b c -> runProgram ps{mask=(a, b, c)} cs 
    Set a b -> runProgram ps{memory=Map.insert a (apply b (mask ps)) $ memory ps} cs
  where apply i (o, z, _) = (i .|. o) .&. complement z

runFloater :: ProgramState -> [Command] -> Memory 
runFloater ps [] = memory ps 
runFloater ps (c:cs) = case c of 
    Mask a b c -> runFloater ps{mask=(a, b, c)} cs 
    Set a b -> runFloater ps{memory = update a b (mask ps) (memory ps)} cs 
  where update a b (o, _, x) mem = foldr (`Map.insert` b) mem [((a .|. o) .&. complement x) .|. i| i <- subs [j | j <- [0..36], x .&. (1 `shiftL` j) /= 0]]
        subs [] = [0]
        subs (j:js) = let re = subs js in re ++ map (\x -> x + (1 `shiftL` j)) re

parse :: String -> Command
parse ('m':'a':'s':'k':' ':'=':' ':s) = 
    let one = map (\c -> if c == 'X' then '0' else c) s 
        zero = map (\c -> if c == '0' then '1' else '0') s
        x = map (\c -> if c == 'X' then '1' else '0') s
        dec = foldl (\acc x -> acc * 2 + digitToInt x) 0
    in Mask (dec one) (dec zero) (dec x)
parse ('m':'e':'m':'[':rest) = Set (read $ takeWhile isDigit rest) (read $ drop 4 $ dropWhile isDigit rest)