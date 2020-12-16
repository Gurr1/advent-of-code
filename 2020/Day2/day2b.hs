import qualified Text.Parsec as Parsec
import ParseUtils
import Data.Either as Either

type Policy = (Int, Int, Char)
type Entry = (Policy, String)

day2b = do
    let file = "input.a"
    input <- readFile file
    output <- calcDay2b $ lines input
    return output
    
calcDay2b input = do 
    let a = map parseEntries input
    let b = occurence True $ map checkConstraintsb a
    return b

checkConstraintsb :: Entry -> Bool
checkConstraintsb ((fst, lst, char), pass) = occurence True [const1, const2] == 1
    where const1 = pass !! (fst-1) == char
          const2 = pass !! (lst-1) == char

occurence ::Eq a => a -> [a] -> Int
occurence char input = length $ filter (==char) input 

parseEntries :: String -> Entry
parseEntries input = case parse parseEntry input of
    Right entry -> entry
    Left e -> error $ show e

parseEntry :: Parsec.Parsec String() Entry
parseEntry = do
    policy <- passwordPolicy
    Parsec.string ": "
    password <- parsePassword
    return (policy, password)

parsePassword :: Parsec.Parsec String() String
parsePassword = Parsec.many1 Parsec.letter

passwordPolicy :: Parsec.Parsec String() Policy
passwordPolicy = do
    min <- parseInt
    Parsec.char '-'
    max <- parseInt
    Parsec.space
    character <- Parsec.letter
    return (min, max, character)