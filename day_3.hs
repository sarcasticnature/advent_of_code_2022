import System.Environment (getArgs)
import System.IO
import Data.List (nub)
import Data.Char (ord, isLower, isUpper)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStr $ show $ scoreDuplicates $ parseForDuplicates contents

splitBackpack :: String -> [String]
splitBackpack [] = []
splitBackpack str = let (x, xs:xs') = span (/= '\n') str
                    in  x : splitBackpack xs'     -- need to get rid of the leading newline

splitPouch :: String -> (String,String)
splitPouch x = splitAt ((length x) `div` 2) x

getPouchDuplicates :: (String,String) -> String
getPouchDuplicates (a,b) = filter (\x -> elem x b) a

parseForDuplicates :: String -> [String]
parseForDuplicates =
    map nub . map getPouchDuplicates . map splitPouch . splitBackpack

scoreItem :: Char -> Int
scoreItem c
    | isLower c = (ord c) - 96
    | isUpper c = (ord c) - 38
    | otherwise = error "bad character in input"

scoreDuplicates :: [String] -> Int
scoreDuplicates dupList =
    let foldFunc acc str = acc + (sum $ map scoreItem str)
    in  foldl foldFunc 0 dupList
