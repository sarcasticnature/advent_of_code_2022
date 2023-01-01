import System.Environment (getArgs)
import System.IO
import Data.List (nub)
import Data.Char (ord, isLower, isUpper)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ scoreBadges $ map getGroupBadge $ groupPacks $ splitBackpack contents

splitBackpack :: String -> [String]
splitBackpack [] = []
splitBackpack str = let (x, xs:xs') = span (/= '\n') str
                    in  x : splitBackpack xs'     -- need to get rid of the leading newline

splitPouch :: String -> (String,String)
splitPouch x = splitAt (length x `div` 2) x

getPairDuplicates :: (String,String) -> String
getPairDuplicates (a,b) = filter (`elem` b) a

parseForDuplicates :: String -> [String]
parseForDuplicates =
    map (nub . getPairDuplicates . splitPouch) . splitBackpack

scoreItem :: Char -> Int
scoreItem c
    | isLower c = ord c - 96
    | isUpper c = ord c - 38
    | otherwise = error "bad character in input"

scoreDuplicates :: [String] -> Int
scoreDuplicates dupList =
    let foldFunc acc str = acc + sum (map scoreItem str)
    in  foldl foldFunc 0 dupList

groupPacks :: [String] -> [(String,String,String)]
groupPacks [] = []
groupPacks (x:y:z:ws) = (x,y,z) : groupPacks ws

getGroupBadge :: (String,String,String) -> Char
getGroupBadge (x,y,z) =
    let dups1 = getPairDuplicates (x,y)
        dups2 = filter (`elem` z) dups1
    in  head dups2

scoreBadges :: [Char] -> Int
scoreBadges = sum . map scoreItem
