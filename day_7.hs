import System.Environment (getArgs)
import System.IO
import Data.List (foldl')

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    --print $ fmap (sumSmallDirs 100000) $ parseInput $ lines contents
    --print $ fmap debugZipper $ parseInput $ lines contents
    print $ fmap (minimumDeleteDirSize 70000000 30000000) $ parseInput $ lines contents

-- Data
data FileSystem a = File String a
                  | Dir String [FileSystem a]
                  deriving (Show,Eq)
type FileSystemInt = FileSystem Int

data FSParent = Parent { name :: String
                       , files :: [FileSystemInt]
                       } deriving (Show)
type FSParents = [FSParent]
type FSZipper = (FSParents, FileSystemInt)

data CDCommand = Descend String
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

-- TODO: make this return Maybe FileSystemInt to handle bad input
parseFSDataLine :: String -> FileSystemInt
parseFSDataLine line =
    if dir
    then Dir name []
    else File name size
    where
        xs = words line
        dir = head xs == "dir"
        name = last xs
        size = read $ head xs

ascend :: FSZipper -> Maybe FSZipper
ascend ([], _) = Nothing
ascend (p:ps, fsData) = Just (ps, Dir (name p) (fsData:files p))

cd :: FSZipper -> CDCommand -> Maybe FSZipper
cd z@([], Dir n fs) GoToRoot = if n == "/" then Just z else Nothing
cd z GoToRoot = case ascend z of
    Just z' -> cd z' GoToRoot
    Nothing -> Nothing
cd z Ascend = ascend z
cd (ps, Dir n fs) (Descend subdir)
    | length matches == 1 = Just (ps', Dir subdir newfs)
    | otherwise = Nothing
    where
        matches = [(n, fs') | (Dir n fs') <- fs, n == subdir]
        newfs = snd $ head matches
        rest = [f | f <- fs, f /= Dir subdir newfs]
        ps' = Parent n rest : ps

cd _ _ = Nothing

-- TODO: use updated parseFSDataLine (which would return Maybe FileSystemInt
parseLS :: FSZipper -> [String] -> Maybe (FSZipper, [String])
parseLS (_, File _ _) _ = Nothing
parseLS (parents, Dir name fs) xs =
    let (ds, xs') = break isFSCommand xs
        foldFn acc line = parseFSDataLine line : acc
        dataList = foldl' foldFn [] ds
        dir = Dir name (dataList ++ fs)
    in  Just ((parents, dir), xs')

munchInputLines :: FSZipper -> [String] -> Maybe (FSZipper, [String])
munchInputLines zipper [] = Just (zipper, [])
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

dirSize :: (Num a) => FileSystem a -> a
dirSize (File _ _) = error "dirSize called on a File instead of a Dir"
dirSize dir = getSize dir
    where getSize (File _ sz) = sz
          getSize (Dir n fs) = sum $ map getSize fs

getDirSizes :: (Num a) => FileSystem a -> [a]
getDirSizes (File _ _) = []
getDirSizes d@(Dir n fs) =
    let rootSize = dirSize d
        getSize [] = []
        getSize ((File _ _):fs') = getSize fs'
        getSize (d'@(Dir _ fs''):fs') = dirSize d' : getSize fs'' ++ getSize fs'
    in  rootSize : getSize fs

sumSmallDirs :: Int -> FSZipper -> Int
sumSmallDirs max z = case cd z GoToRoot of
    Nothing           -> 0
    Just (_, rootDir) -> sum $ filter (< max) $ getDirSizes rootDir

minimumDeleteDirSize :: Int -> Int -> FSZipper -> Int
minimumDeleteDirSize maxSize updateSize z = case cd z GoToRoot of
    Nothing -> 0
    Just (_, rootDir) -> let dirs = getDirSizes rootDir
                             rootSize = head dirs
                             minDirSize = updateSize - (maxSize - rootSize)
                         in  minimum $ filter (> minDirSize) dirs

-- debugging
debugZipper z = case cd z GoToRoot of
    Nothing           -> []
    Just (_, rootDir) -> getDirInfo rootDir

getDirInfo :: (Num a) => FileSystem a -> [(String, a)]
getDirInfo (File _ _) = []
getDirInfo d@(Dir n fs) =
    let rootInfo = (n, dirSize d)
        getInfo [] = []
        getInfo ((File _ _):fs') = getInfo fs'
        getInfo (d'@(Dir n' fs''):fs') = (n', dirSize d') : getInfo fs'' ++ getInfo fs'
    in  rootInfo : getInfo fs
