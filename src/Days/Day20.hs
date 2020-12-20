module Days.Day20 where
import Prelude hiding (flip)
import Data.List.Split
import Data.List
import Data.Char
import Debug.Trace

type Edge = [Bool]
type Grid = [Edge]
data Tile = Tile Int Grid
instance Eq Tile where
    a == b = tid a == tid b
instance Show Tile where 
    show (Tile _ g) = "\n" ++ concat [map (\b -> if b then '#' else ' ') line ++ "\n" | line <- g]

safeHead :: [a] -> Maybe a 
safeHead [] = Nothing 
safeHead xs = Just $ head xs

seaMonster :: [(Int, Int)]
seaMonster = [(0,18),(1,0),(1,5),(1,6),(1,11),(1,12),(1,17),(1,18),(1,19),(2,1),(2,4),(2,7),(2,10),(2,13),(2,16)]

interface :: Tile -> [Edge]
interface (Tile _ grid) = [last $ transpose grid, last grid, head $ transpose grid, head grid]

tid :: Tile -> Int 
tid (Tile i _) = i

grid :: Tile -> [[Bool]]
grid (Tile _ g) = g

normalize :: Edge -> Edge 
normalize e = min e (reverse e)

rotate :: Tile -> Tile 
rotate (Tile i g) = Tile i (transpose $ map reverse g)

flipUD :: Tile -> Tile 
flipUD (Tile i g) = Tile i (transpose . map reverse . transpose $ g)

flipLR :: Tile -> Tile 
flipLR (Tile i g) = Tile i (map reverse g)

match :: Tile -> Tile -> Bool 
match tile other = not $ null $ intersect (map normalize $ interface tile) (map normalize $ interface other)

matchRight :: Tile -> Tile -> Bool 
matchRight tile other = elem (normalize $ interface other !! 0) (map normalize $ interface tile) 

flipToMatchAbove :: Tile -> Tile -> Tile 
flipToMatchAbove above tile | normalize (interface above !! 1) == normalize (interface tile !! 3) = tile 
                            | otherwise = flipUD tile

rotateToMatchAbove :: Tile -> Tile -> Tile 
rotateToMatchAbove above tile | normalize (interface above !! 1) == normalize (interface tile !! 3) = tile 
                              | otherwise = rotateToMatchAbove above $ rotate tile

flipToMatchLeft :: Tile -> Tile -> Tile 
flipToMatchLeft before tile | normalize (interface before !! 0) == normalize (interface tile !! 2) = tile 
                            | otherwise = flipLR tile

run :: String -> String 
run s = 
    let tiles = map parseTile $ splitOn "\n\n" s
        uniqs = uniqueEdges tiles
        corners = [tile | tile <- tiles, isCorner tile uniqs]
        res1 = product $ map tid corners
        combined = rotate $ rotate $ joinGrid $ buildGrid tiles uniqs Nothing
        n = length (grid combined)
        seaPos = length $ nub $ concat [seaMonsterPositions (y, x) combined | y <- [0..n-1], x <- [0..n-1]]
        hashPos = length $ filter id $ concat $ grid combined
    in show res1 ++ "\n" ++ show (hashPos - seaPos)

select :: [Tile] -> [Edge] -> Maybe Tile -> Maybe Tile -> Maybe Tile 
select tiles uniqs left top = case (left, top) of 
                                (Nothing, Nothing) -> Just (firstCorner tiles uniqs)
                                (Nothing, Just above) -> Just (firstBorder tiles uniqs above)
                                (Just before, Nothing) -> nextTop tiles uniqs before
                                (Just before, Just above) -> nextInside tiles uniqs before above

uniqueEdges :: [Tile] -> [Edge]
uniqueEdges tiles = map head $ filter (\l -> length l == 1) $ group $ sort $ map normalize $ concatMap interface tiles

isCorner :: Tile -> [Edge] -> Bool 
isCorner tile uniqs = length (filter (`elem` uniqs) (map normalize $ interface tile)) == 2

isBorder :: Tile -> [Edge] -> Bool 
isBorder tile uniqs = length (filter (`elem` uniqs) (map normalize $ interface tile)) == 1

firstCorner :: [Tile] -> [Edge] -> Tile 
firstCorner tiles uniqs = 
    let corner = head [tile | tile <- tiles, isCorner tile uniqs]
    in rotateCorner uniqs corner 
  where rotateCorner uniqs tile | length (intersect (drop 2 $ map normalize $ interface tile) uniqs) == 2 = tile 
                                | otherwise = rotateCorner uniqs (rotate tile)

firstBorder :: [Tile] -> [Edge] -> Tile -> Tile 
firstBorder tiles uniqs above = 
    let border = head $ [tile | tile <- tiles, isBorder tile uniqs || isCorner tile uniqs, match tile above]
    in flipToMatchAbove above $ rotateBorder uniqs border 
  where rotateBorder uniqs tile | elem (normalize $ interface tile !! 2) uniqs = tile 
                                | otherwise = rotateBorder uniqs (rotate tile)

nextTop :: [Tile] -> [Edge] -> Tile -> Maybe Tile 
nextTop tiles uniqs before = 
    let mTile = safeHead $ [tile | tile <- tiles, isBorder tile uniqs || isCorner tile uniqs, matchRight tile before]
    in case mTile of 
        Nothing -> Nothing 
        Just top -> Just $ flipToMatchLeft before $ rotateBorder uniqs top 
  where rotateBorder uniqs tile | elem (normalize $ interface tile !! 3) uniqs && (matchRight before tile || matchRight before (flipLR tile)) = tile 
                                | otherwise = rotateBorder uniqs (rotate tile)

nextInside :: [Tile] -> [Edge] -> Tile -> Tile -> Maybe Tile 
nextInside tiles uniqs before above = 
    let mTile = safeHead $ [tile | tile <- tiles, match tile before, match tile above]
    in case mTile of 
        Nothing -> Nothing 
        Just tile -> Just $ flipToMatchLeft before $ rotateToMatchAbove above tile 

buildRow :: [Tile] -> [Edge] -> Maybe Tile -> Maybe [Tile] -> [Tile]
buildRow _ _ _ (Just []) = []
buildRow tiles uniqs left prev = 
    let sel = select tiles uniqs left (head <$> prev)
    in case sel of
        Nothing -> []
        Just first -> first : buildRow (delete first tiles) uniqs (Just first) (tail <$> prev)

buildGrid :: [Tile] -> [Edge] -> Maybe [Tile] -> [[Tile]]
buildGrid [] _ _ = []
buildGrid tiles uniqs prev = 
    let first = buildRow tiles uniqs Nothing prev 
    in first : buildGrid (foldr delete tiles first) uniqs (Just first)

joinGrid :: [[Tile]] -> Tile
joinGrid tiles = Tile 0 (join $ map (map reduce) tiles) 
  where reduce (Tile _ g) = tail . init . transpose . tail . init . transpose $ g
        n = length (head tiles)
        k = length (grid $ head $ head tiles) - 2
        join bbs = [[bbs !! i !! j !! y !! x | j <- [0..n-1], x <- [0..k-1]] | i <- [0..n-1], y <- [0..k-1]]

seaMonsterPositions :: (Int, Int) -> Tile -> [(Int, Int)]
seaMonsterPositions (y, x) (Tile _ g) =
    let n = length g 
        pos = [(y + i, x + j)| (i, j) <- seaMonster]
    in if y + 2 >= n || x + 19 >= n then [] 
         else if all (\p -> g !! fst p !! snd p) pos then pos else []


parseTile :: String -> Tile 
parseTile s = Tile (read $ filter isDigit $ head $ lines s) (map (map (== '#')) $ tail $ lines s)