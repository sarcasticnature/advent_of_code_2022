import System.Environment (getArgs)
import System.IO
import Data.List (zip4)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ findStartPacket contents

unique4 :: (Char,Char,Char,Char) -> Bool
unique4 (x,y,z,w) =
    (x /= y) && (x /= z) && (x /= w) && (y /= z) && (y /= w) && (z /= w)

findStartPacket :: String -> Int
findStartPacket xs =
    let ys = tail xs
        zs = tail ys
        ws = tail zs
        packetZip = zip4 xs ys zs ws
    in  length (takeWhile (not . unique4) packetZip) + 4

debug xs =
    let ys = tail xs
        zs = tail ys
        ws = tail zs
    in  zip4 xs ys zs ws
