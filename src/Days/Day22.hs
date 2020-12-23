module Days.Day22 where 

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set

run :: String -> String 
run s = let (xs, ys) = parse s 
            (xf, _) = play (xs, ys)
            res1 = score xf
            (xff, _) = playRec Set.empty (xs, ys)
            res2 = score xff
        in show res1 ++ ", " ++ show res2

parse :: String -> (Seq Int, Seq Int)
parse s = (Seq.fromList $ map read $ take 25 $ drop 1 $ lines s, Seq.fromList $ map read $ drop 28 $ lines s)

play :: (Seq Int, Seq Int) -> (Seq Int, Bool)
play st = case st of 
    (Empty, ys) -> (ys, False)
    (xs, Empty) -> (xs, True)
    (x:<|xs, y:<|ys) -> if x > y then play (xs:|>x:|>y, ys) else play (xs, ys:|>y:|>x)

playRec :: Set [Int] -> (Seq Int, Seq Int) -> (Seq Int, Bool)
playRec seen st = case st of 
    (Empty, ys) -> (ys, False)
    (xs, Empty) -> (xs, True)
    (sx@(x:<|xs), y:<|ys) -> 
        if elem (toList sx) seen then (sx, True)
        else let win = if x > Seq.length xs || y > Seq.length ys
                    then x > y 
                    else snd $ playRec Set.empty (Seq.take x xs, Seq.take y ys)
             in playRec (Set.insert (toList sx) seen) (if win then (xs:|>x:|>y, ys) else (xs, ys:|>y:|>x))


score :: Seq Int -> Int 
score seq = sum [i * v | (i, v) <- zip [1..] $ reverse $ toList seq]
