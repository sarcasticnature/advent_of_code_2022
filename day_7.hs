import System.Environment (getArgs)
import System.IO
import Data.List (foldl')

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ reverse $ testParseFSDataLine $ lines contents

-- Data
type FSSize = Int
type FSName = String
data FSData = File FSName FSSize
            | Dir FSName [FSData]
            deriving (Show)

data FSParent = Parent FSName [FSData] deriving (Show)
type FSParents = [FSParent]
type FSZipper = (FSParents, FSData)

data CDCommand = Descend FSName
               | Ascend
               | GoToRoot
               deriving (Show)
data FSCommand = CD CDCommand
               | LS
               deriving (Show)

-- Parsing
isFSCommand :: String -> Bool
isFSCommand = (==) '$' . head

parseCommandLine :: String -> FSCommand
parseCommandLine line = case words line of
    [_, cmd]     -> LS
    [_, cmd, dir] -> getCDCommand dir
    where
        getCDCommand dir
            | dir == "/" = CD GoToRoot
            | dir == ".." = CD Ascend
            | otherwise = CD (Descend dir)

parseFSDataLine :: String -> FSData
parseFSDataLine line =
    if dir
    then Dir name []
    else File name size
    where
        xs = words line
        dir = head xs == "dir"
        name = last xs
        size = read $ head xs

testParseCmd :: [String] -> [FSCommand]
testParseCmd lines =
    let cmdLines = filter isFSCommand lines
        foldFn acc cmd = parseCommandLine cmd : acc
    in  foldl' foldFn [] cmdLines

testParseFSDataLine :: [String] -> [FSData]
testParseFSDataLine lines =
    let dataLines = filter (not . isFSCommand) lines
        foldFn acc line = parseFSDataLine line : acc
    in  foldl' foldFn [] dataLines
