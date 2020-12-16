import qualified Text.Parsec as Parsec
import ParseUtils
import Data.Either as Either

type Policy = (Int, Int, Char)
type Entry = (Policy, String)

day2a = do
    let file = "input.a"
    input <- readFile file
    let a = map parseEntries $ lines input
    let b = occurence True $ map checkConstraints a
    return b
    

checkConstraints :: Entry -> Bool
checkConstraints ((min, max, char), pass) = occ >= min && occ <= max
    where occ = occurence char pass

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