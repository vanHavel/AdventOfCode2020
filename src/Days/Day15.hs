{-# LANGUAGE BangPatterns #-}

module Days.Day15 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)

run :: String -> String 
run s = let nInit = map read $ splitOn "," s 
            mInit = Map.fromList $ zip nInit [1..]
            nums = nInit ++ seque mInit (last nInit) (length nInit)
        in show (nums !! 2019) ++ ", " ++ show (nums !! (30000000 - 1))

seque :: Map Int Int -> Int -> Int -> [Int]
seque !m c i = let next = case Map.lookup c m of 
                      Nothing -> 0 
                      Just j  -> i - j
               in next : seque (Map.insert c i m) next (succ i)