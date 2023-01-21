import System.Environment (getArgs)
import System.IO
import Data.List (foldl', transpose)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ maxViewDistance $ pairsToNormals $ removeHidden $ linesToTreePairs $ lines contents
    --putStrLn $ prettyForest $ removeHidden $ linesToTreePairs $ lines contents

type TreePair = (Bool, Char)
type TreeNormals = ([Char], [Char], [Char], [Char], Char)

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

normals :: Int -> Int -> [[TreePair]] -> TreeNormals
normals x y zs =
    let row = zs !! x
        col = transpose zs !! y
        (as, bs) = splitAt y row
        (cs, ds) = splitAt x col
        height = snd $ row !! y
        as' = map snd $ reverse as
        bs' = map snd $ drop 1 bs
        cs' = map snd $ reverse cs
        ds' = map snd $ drop 1 ds
    in  (as', bs', cs', ds', height)

pairsToNormals :: [[TreePair]] -> [TreeNormals]
pairsToNormals xs =
    let size = length xs - 1
        idxs = [(a, b) | a <- [0..size], b <- [0..size]]
        f acc (x, y) = normals x y xs : acc
    in  foldl' f [] idxs

sightline :: (Char -> Bool) -> [Char] -> Int
sightline f xs =
    let (ys, zs) = span f xs
        carry = if null zs then 0 else 1
    in  length ys + carry

viewDistance :: TreeNormals -> Int
viewDistance (as,bs,cs,ds,height) =
    let a = sightline (< height) as
        b = sightline (< height) bs
        c = sightline (< height) cs
        d = sightline (< height) ds
    in  a * b * c * d

maxViewDistance :: [TreeNormals] -> Int
maxViewDistance = maximum . map viewDistance

-- debugging

prettyForest :: [[TreePair]] -> String
prettyForest xs =
    let foldFn acc (b, c) = if b then acc ++ [c] else acc ++ ['.']
    in  unlines $ map (foldl' foldFn []) xs

prettyBools :: [[TreePair]] -> String
prettyBools xs =
    let foldFn acc (b, c) = if b then acc ++ "T" else acc ++ "F"
    in  unlines $ map (foldl' foldFn []) xs
