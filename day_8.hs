import System.Environment (getArgs)
import System.IO
import Data.List (foldl', transpose)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ countVisible $ removeHidden $ linesToTreePairs $ lines contents

type TreePair = (Bool, Char)

linesToTreePairs :: [[Char]] -> [[TreePair]]
linesToTreePairs = map (zip $ repeat False)

-- Note that '.' is LT <some number char> (i.e. '.' < '0' == True)
foldVisible :: [TreePair] -> [TreePair]
foldVisible [] = []
foldVisible xs =
    let append xs y = xs ++ [y]
        visible :: ([TreePair], Char) -> TreePair -> ([TreePair], Char)
        visible (acc, m) (b, c)
            | b == True = (append acc (b, c), m')
            | c > m   = (append acc (True, c), m')
            | otherwise = (append acc (False, c), m')
                where m' = max m c
        (xs', _) = foldl' visible ([], '.') xs
    in  xs'

removeHidden :: [[TreePair]] -> [[TreePair]]
removeHidden forest =
    prunedMap
    where
        rows = map foldVisible forest
        rows' = map (reverse . foldVisible . reverse) rows
        cols = map foldVisible $ transpose rows'
        prunedMap = transpose $ map (reverse . foldVisible . reverse) cols

countVisible :: [[TreePair]] -> Int
countVisible xs =
    let bools = map (map fst) xs
        counts = map (length . filter id) bools
    in  sum counts

-- debugging

prettyForest :: [[TreePair]] -> [String]
prettyForest xs =
    let foldFn acc (b, c) = if b then acc ++ [c] else acc ++ ['.']
    in  map (foldl' foldFn []) xs
