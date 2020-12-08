module Days.Day7 where

import Text.Parsec
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Either

run :: String -> String 
run s = 
    let parsed = parseInput s 
        res1 = contains "shiny gold" parsed
        res2 = inside "shiny gold" parsed
    in show res1 ++ ", " ++ show res2

contains :: String -> Map String [(String, Int)] -> Int 
contains s m = length . filter id $ [contains1 s m b | b <- Map.keys m]
  where contains1 s m b = case m Map.! b of
          [] -> False
          subs -> any ((==s) . fst) subs || maximum [contains1 s m c | c <- map fst subs]

inside :: String -> Map String [(String, Int)] -> Int 
inside s m = case m Map.! s of
    [] -> 0
    subs -> sum [cnt + cnt * inside nm m | (nm, cnt) <- subs]

parseInput :: String -> Map String [(String, Int)]
parseInput = Map.fromList . map (fromRight undefined . parse parseLine "") . lines
  where parseLine :: Parsec String u (String, [(String, Int)])
        parseLine = do 
            name <- manyTill (letter <|> space) (try $ string " bags ")
            string "contain"
            rest <- option [] $ try $ many1 $ do
                space
                cnt <- read . (:[]) <$> digit 
                space 
                nm <- manyTill (letter <|> space) (try $ string " bag")
                optional (char 's')
                oneOf ".,"
                return (nm, cnt)
            return (name, rest)