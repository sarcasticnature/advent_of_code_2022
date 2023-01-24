import System.Environment (getArgs)
import System.IO
import Data.List (foldl')

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ sumInterestingSignalStrengths $ parseInstructions $ lines contents
    --print $ listInterestingSignalStrengths $ parseInstructions $ lines contents
    --print $ listRegx $ parseInstructions $ lines contents

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

interesting :: Int -> Int -> Int
interesting i n
    | i == 20                          = strength i n
    | i > 20 && (i - 20) `mod` 40 == 0 = strength i n
    | otherwise                        = 0
    where
        strength = (*)

sumInterestingSignalStrengths :: [MaybeAdd] -> Int
sumInterestingSignalStrengths ms =
    let fold (acc, regx, i) m = case m of
            Nothing -> (acc + interesting i regx, regx, i + 1)
            Just x  -> (acc + interesting i regx, regx + x, i + 1)
        (sum, _, _) = foldl' fold (0, 1, 1) ms
    in sum

-- Debug

listInterestingSignalStrengths ms =
    let strength = (*)
        interesting' i n = if interesting i n /= 0 then [interesting i n] else []
        fold (acc, regx, i) m = case m of
            Nothing -> (acc ++ interesting' i regx, regx, i + 1)
            Just x  -> (acc ++ interesting' i regx, regx + x, i + 1)
        (list, _, _) = foldl' fold ([], 1, 1) ms
    in list

listRegx ms =
    let fold (acc, regx, i) m = case m of
            Nothing -> (acc ++ [regx], regx, i + 1)
            Just x  -> (acc ++ [regx], regx + x, i + 1)
        (list, _, _) = foldl' fold ([], 1, 1) ms
    in zip list [1..]
