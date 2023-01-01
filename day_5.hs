import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ parseInput $ lines contents

-- Parsing stuff

data Command = Command { count :: Int
                       , start :: Int
                       , end :: Int
                       } deriving (Show)

type CrateStack = [Char]
type CrateList = [CrateStack]

isCrateLine :: String -> Bool
isCrateLine [] = False
isCrateLine xs = '[' `elem` xs

chunkCrateLine :: String -> [String]
chunkCrateLine [] = []
chunkCrateLine xs = take 4 xs : chunkCrateLine (drop 4 xs)

lineToCrates :: String -> [Char]
lineToCrates str =
    let chunks = chunkCrateLine str
    in  [ x !! 1 | x <- chunks]

createCrateList :: Int -> CrateList
createCrateList 0 = []
createCrateList n = "" : createCrateList (n - 1)

stackCrates :: CrateList -> String -> CrateList
stackCrates crateList line =
    let crateZip = zip crateList line
        addCrate (xs,c)
            | c == ' '  = xs
            | otherwise = xs ++ [c]
    in  map addCrate crateZip

lineToCommand :: String -> Command
lineToCommand str =
    let chunks = words str
        c = read $ chunks !! 1
        s = read $ chunks !! 3
        e = read $ last chunks
    in  Command c s e

parseInput :: [String] -> (CrateList, [Command])
parseInput xs =
    let crateLines = map lineToCrates $ takeWhile isCrateLine xs
        blankCrateList = createCrateList $ length $ head crateLines
        crateList = foldl stackCrates blankCrateList crateLines
        commands = map lineToCommand $ tail $ tail $ dropWhile isCrateLine xs
    in (crateList, commands)

-- Algorithm stuff

