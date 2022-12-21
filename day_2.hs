import System.Environment (getArgs)
import System.IO
import Data.Char (isAlphaNum,isSpace)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStr $ show $ splitGames contents

splitGames :: String -> [String]
splitGames [] = []
splitGames str = let (x, xs:xs') = span (/= '\n') str
                 in  x : splitGames xs'     -- need to get rid of the leading newline

