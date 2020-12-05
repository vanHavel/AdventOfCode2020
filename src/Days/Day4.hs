module Days.Day4 where 

import Data.Char
import Data.List.Split ( splitOn, splitOneOf )
import Data.Map (Map)
import qualified Data.Map as Map

run :: String -> String 
run s = 
    let files = map parse $ splitOn "\n\n" s 
        res1 = length $ filter isComplete files 
        res2 = length . filter isValid $ filter isComplete files
    in show res1 ++ ", " ++ show res2

parse :: String -> Map String String 
parse = Map.fromList . map ((\[x, y] -> (x, y)) . splitOn ":") . filter (/= "") . splitOneOf " \n"

isComplete :: Map String String -> Bool
isComplete m = minimum [Map.member k m | k <- ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]]

isValid :: Map String String -> Bool 
isValid m = minimum [
    p (m Map.! k) | (k, p) <- [
        ("byr", (\i -> i >= 1920 && i <= 2002) . read),
        ("iyr", (\i -> i >= 2010 && i <= 2020) . read),
        ("eyr", (\i -> i >= 2020 && i <= 2030) . read),
        ("hgt", \s -> let (num, unit) = span isDigit s in case unit of 
            "in" -> 59 <= read num && read num <= 76
            "cm" -> 150 <= read num && read num <= 193
            _ -> False),
        ("hcl", \s -> head s == '#' && all (`elem` "0123456789abcdef") (tail s) && length s == 7),
        ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
        ("pid", \n -> all isDigit n && length n == 9)
    ]]