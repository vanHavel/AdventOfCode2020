module Days.Day2 where

import Text.Parsec

run :: String -> String 
run s =
    let samples = map parseSample $ lines s
        validCount = length $ filter isValid samples
        validCount' = length $ filter isValid' samples
    in show validCount ++ ", " ++ show validCount'

parseSample :: String -> (Int, Int, Char, String)
parseSample s = let (Right result) = parse lineParser "" s in result

lineParser = do
    low <- read <$> many1 digit
    char '-'
    up <- read <$> many1 digit
    space
    c <- lower 
    char ':'
    space 
    password <- many1 letter 
    return (low, up, c, password)

isValid :: (Int, Int, Char, String) -> Bool 
isValid (low, up, c, s) = low <= occurrences && occurrences <= up
  where occurrences = length [d | d <- s, d == c]

isValid' :: (Int, Int, Char, String) -> Bool 
isValid' (low, up, c, s) = (s !! (low - 1) == c) /= (s !! (up - 1) == c)