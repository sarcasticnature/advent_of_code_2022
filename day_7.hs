import System.Environment (getArgs)
import System.IO
import Data.List (foldl')

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ lines contents

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

parseCommandLine :: String -> Maybe FSCommand
parseCommandLine line = case words line of
    [_, cmd]      -> if cmd == "ls" then Just LS else Nothing
    [_, cmd, dir] -> if cmd == "cd" then Just (getCDCommand dir) else Nothing
    _             -> Nothing
    where
        getCDCommand dir
            | dir == "/" = CD GoToRoot
            | dir == ".." = CD Ascend
            | otherwise = CD (Descend dir)

-- TODO: make this return Maybe FSData to handle bad input
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

--testParseCmd :: [String] -> [FSCommand]
--testParseCmd lines =
--    let cmdLines = filter isFSCommand lines
--        foldFn acc cmdLine = case parseCommandLine cmdLine of Just cmd -> cmd:acc
--                                                              Nothing  -> acc
--    in  foldl' foldFn [] cmdLines
--
--testParseFSDataLine :: [String] -> [FSData]
--testParseFSDataLine lines =
--    let dataLines = filter (not . isFSCommand) lines
--        foldFn acc line = parseFSDataLine line : acc
--    in  foldl' foldFn [] dataLines

cd :: FSZipper -> CDCommand -> Maybe FSZipper
cd (parents, fsData) GoToRoot = Nothing
cd (parents, fsData) Ascend = Nothing
cd (parents, Dir d ss) (Descend subdir) = Nothing
cd _ _ = Nothing

-- TODO: use updated parseFSDataLine (which would return Maybe FSData
parseLS :: FSZipper -> [String] -> Maybe (FSZipper, [String])
parseLS (_, File _ _) _ = Nothing
parseLS (parents, Dir name ys) xs =
    let (ds, xs') = break isFSCommand xs
        foldFn acc line = parseFSDataLine line : acc
        dataList = foldl' foldFn [] ds
        dir = Dir name (dataList ++ ys)
    in  Just ((parents, dir), xs')

munchInputLines :: FSZipper -> [String] -> Maybe (FSZipper, [String])
munchInputLines zipper [] = Just (zipper, [])   -- careful: this could lead to an infinite loop
munchInputLines zipper (x:xs) = case parseCommandLine x of
    Nothing       -> Nothing
    Just LS       -> parseLS zipper xs
    Just (CD cmd) -> case cd zipper cmd of Just zipper' -> Just (zipper', xs)
                                           Nothing      -> Nothing

parseInput :: [String] -> Maybe FSZipper
parseInput xs =
    let parse :: FSZipper -> [String] -> Maybe FSZipper
        parse zipper [] = Just zipper
        parse zipper ys = case munchInputLines zipper ys of
            Just (zipper', zs) -> parse zipper' zs
            Nothing            -> Nothing
    in  parse ([], Dir "/" []) xs

-- algorithm
