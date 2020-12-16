import Debug.Trace
module Day1b (day1b) where

day1bcalc :: [Int] -> Int
day1bcalc xs = head $ map (\(a, b, c) -> a*b*c) $ filter (\(a, b, c) -> a+b+c==2020) tuple
    where tuple = [(a, b, c) | a <- xs, b <- xs, c <-xs]

day1b = do
    let file = "input.a"
    input <- readFile file
    return (day1 $ map read $ lines input)


