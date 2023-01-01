import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ numTrue $ map (isPartiallyOverlapping . parseAssignment) $ lines contents

data Assignment = Assignment { start1 :: Int
                             , end1 :: Int
                             , start2 :: Int
                             , end2 :: Int }

parseAssignment :: String -> Assignment
parseAssignment str =
    let getStart = read . takeWhile (/= '-') :: String -> Int
        getEnd = read . tail . dropWhile (/= '-') :: String -> Int
        (first, _:second) = span (/= ',') str
        start1 = getStart first
        end1 = getEnd first
        start2 = getStart second
        end2 = getEnd second
    in  Assignment start1 end1 start2 end2

isFullyOverlapping :: Assignment -> Bool
isFullyOverlapping (Assignment s1 e1 s2 e2) =
    (s1 <= s2) && (e1 >= e2) || (s2 <= s1) && (e2 >= e1)

isPartiallyOverlapping :: Assignment -> Bool
isPartiallyOverlapping (Assignment s1 e1 s2 e2) =
    let overlap1 = (e1 >= s2) && (s1 <= s2)
        overlap2 = (e2 >= s1) && (s2 <= s1)
    in  overlap1 || overlap2

numTrue :: [Bool] -> Int
numTrue = length . filter id
