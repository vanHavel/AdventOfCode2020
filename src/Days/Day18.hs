module Days.Day18 where

import Data.Char
run :: String -> String 
run s = let stripped = lines $ filter (/= ' ') s 
            res1 = sum $ map (eval []) stripped
            res2 = sum $ map (eval2 []) stripped
        in show res1 ++ ", " ++ show res2

eval :: [Either Int Char] -> [Char] -> Int
eval [Left x] [] = x
eval stack (c:cs) = case c of 
            '(' -> eval (Right '(':stack) cs 
            ')' -> case stack of 
                    (Left v):(Right '('):(Right op):(Left w):rest -> eval (Left (apply v op w):rest) cs
                    (Left v):(Right '('):rest -> eval (Left v:rest) cs
            '+' -> eval (Right '+':stack) cs
            '*' -> eval (Right '*':stack) cs
            d -> let v = digitToInt d in case stack of 
                    (Right op):(Left w):rest -> eval (Left (apply v op w):rest) cs
                    stack -> eval (Left v:stack) cs
  where apply v '+' w = v + w 
        apply v '*' w = v * w

eval2 :: [Either Integer Char] -> [Char] -> Integer
eval2 [Left x] [] = x
eval2 ((Left v):(Right '*'):(Left w):rest) [] = eval2 (Left (v * w):rest) []
eval2 stack (c:cs) = case c of 
            '(' -> eval2 (Right '(':stack) cs 
            ')' -> case stack of 
                    (Left v):(Right '*'):(Left w):rest -> eval2 (Left (v * w):rest) (c:cs)
                    (Left v):(Right '('):(Right '+'):(Left w):rest -> eval2 (Left (v + w):rest) cs
                    (Left v):(Right '('):rest -> eval2 (Left v:rest) cs
            '+' -> eval2 (Right '+':stack) cs
            '*' -> eval2 (Right '*':stack) cs
            d -> let v = fromIntegral $ digitToInt d in case stack of 
                    (Right '+'):(Left w):rest -> eval2 (Left (v + w):rest) cs
                    stack -> eval2 (Left v:stack) cs
        