import Debug.Trace

day1 :: [Int] -> Int
day1 xs = head $ map (\(a, b) -> a*b) $ filter (\(a, b) -> a+b==2020) tuple
    where tuple = [(a, b) | a <- xs, b <- xs]

main = do
    let file = "input.a"
    input <- readFile file
    return (day1 $ map read $ lines input)
