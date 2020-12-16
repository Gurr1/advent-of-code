import Debug.Trace
module Day1 (day1a) where 


day1acalc :: [Int] -> Int
day1acalc xs = head $ map (\(a, b) -> a*b) $ filter (\(a, b) -> a+b==2020) tuple
    where tuple = [(a, b) | a <- xs, b <- xs]

day1a = do
    let file = "input.a"
    input <- readFile file
    return (day1acalc $ map read $ lines input)
