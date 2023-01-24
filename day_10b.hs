import System.Environment (getArgs)
import System.IO
import Data.List (foldl')

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn $ unlines $ calculatePixels $ parseInstructions $ lines contents
    --print $ listPixelRegx $ parseInstructions $ lines contents

-- Data

type MaybeAdd = Maybe Int

-- Parsing

parseOp :: String -> [MaybeAdd]
parseOp xs = case words xs of
    []       -> [Nothing]
    [noop]   -> [Nothing]
    [addx,n] -> [Nothing, Just (read n)]
    _        -> [Nothing]

parseInstructions :: [String] -> [MaybeAdd]
parseInstructions xs =
    let parse acc x = acc ++ parseOp x
    in  foldl' parse [] xs
-- Algorithm

munch :: Int -> [a] -> [[a]]
munch _ [] = []
munch n xs = [take n xs] ++ munch n (drop n xs)

calculatePixels :: [MaybeAdd] -> [String]
calculatePixels ms =
    let inView a b = abs (a - b) <= 1
        pixelChar x i = if inView x i then "#" else "."
        fold (acc, regx, i) m = case m of
            Nothing -> (acc ++ pixelChar regx i, regx, (i + 1) `mod` 40)
            Just x  -> (acc ++ pixelChar regx i, regx + x, (i + 1) `mod` 40)
        (pixels, _, _) = foldl' fold ("", 1, 0) ms
    in munch 40 pixels

-- Debug

listRegx ms =
    let fold (acc, regx, i) m = case m of
            Nothing -> (acc ++ [regx], regx, i + 1)
            Just x  -> (acc ++ [regx], regx + x, i + 1)
        (list, _, _) = foldl' fold ([], 1, 0) ms
    in zip list [1..]

listPixelRegx ms =
    let fold (acc, regx, i) m = case m of
            Nothing -> (acc ++ [regx], regx, (i + 1) `mod` 40)
            Just x  -> (acc ++ [regx], regx + x, (i + 1) `mod` 40)
        (list, _, _) = foldl' fold ([], 1, 0) ms
    in zip list [1..]
