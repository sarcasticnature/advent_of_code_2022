import System.Environment (getArgs)
import System.IO
import Data.List (foldl')
import qualified Data.Set as Set

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ Set.size $ runMotions $ parseMotions $ lines contents
    --print $ reverse $ listMotions $ parseMotions $ lines contents

-- Data

data Motion = U | D | R | L deriving (Show)
type Position = (Int, Int)
type PositionSet = Set.Set Position
type Rope = (Position, Position)

-- Parsing

lineToMotions :: String -> [Motion]
lineToMotions xs
    | dir == 'U' = take n $ repeat U
    | dir == 'D' = take n $ repeat D
    | dir == 'R' = take n $ repeat R
    | dir == 'L' = take n $ repeat L
    where
        dir = head xs
        n = read $ last $ words xs

parseMotions :: [String] -> [Motion]
parseMotions =
    reverse . foldl' f []
    where f acc xs = lineToMotions xs ++ acc

-- Algorithm

moveEnd :: Motion -> Position -> Position
moveEnd U (x,y) = (x + 1, y)
moveEnd D (x,y) = (x - 1, y)
moveEnd R (x,y) = (x, y + 1)
moveEnd L (x,y) = (x, y - 1)

moves :: Rope -> [Motion]
moves ((xh,yh),(xt,yt))
    | abs dx > 1 = vDir dx ++ hDir dy
    | abs dy > 1 = vDir dx ++ hDir dy
    | otherwise  = []
    where
        dx = xh - xt
        dy = yh - yt
        vDir x | x > 0 = [U] | x < 0  = [D] | otherwise = []
        hDir y | y > 0 = [R] | y < 0 = [L] | otherwise = []

moveTail :: Rope -> Position
moveTail r@(_,t) = case moves r of
    [] -> t
    [m] -> moveEnd m t
    [m1, m2] -> moveEnd m2 $ moveEnd m1 t

runMotions :: [Motion] -> PositionSet
runMotions xs =
    let origin = ((0,0),(0,0), Set.empty)
        moveRope (h, t, s) m = (h', t', s')
            where
                h' = moveEnd m h
                t' = moveTail (h', t)
                s' = Set.insert t' s
        (_, _, set) = foldl' moveRope origin xs
    in  set

-- Debug

listMotions xs =
    let origin = [((0,0),(0,0))]
        moveRope acc@((h, t):_) m = (moveEnd m h, moveTail (moveEnd m h, t)) : acc
    in  foldl' moveRope origin xs
