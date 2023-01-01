import System.Environment (getArgs)
import System.IO
import Data.List (zip4)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ findStartPacket 4 contents

findUniques :: Int -> (String, Int) -> Char -> (String, Int)
findUniques n (str, i) c
    | length str > n = (str, i)
    | length str == n = if unique str
                            then (c:str, i)
                            else (c : init str, i + 1)
    | otherwise = (c:str, i + 1)
    where unique (x:y:z:w:_) = (x /= y) && (x /= z) && (x /= w) && (y /= z) && (y /= w) && (z /= w)

findStartPacket :: Int -> String -> Int
findStartPacket n xs = index where (acc, index) = foldl (findUniques n) ([], 0) xs
