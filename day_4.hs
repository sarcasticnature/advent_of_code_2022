import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ numTrue $ map isFullyOverlapping $ lines contents

-- could use the standard library isInfixOf instead (it's probably better)
--isSublist :: Eq a => [a] -> [a] -> Bool
--isSublist needle [] = False
--isSublist needle stack =
--    (needle == take (length needle) stack) || isSublist needle (tail stack)

isFullyOverlapping :: String -> Bool
isFullyOverlapping str = 
    (start1 <= start2) && (end1 >= end2) || (start2 <= start1) && (end2 >= end1)
    where (first, _:second) = span (/= ',') str
          getStart = read . takeWhile (/= '-') :: String -> Int
          getEnd = read . tail . dropWhile (/= '-') :: String -> Int
          start1 = getStart first
          end1 = getEnd first
          start2 = getStart second
          end2 = getEnd second

numTrue :: [Bool] -> Int
numTrue = length . filter id
