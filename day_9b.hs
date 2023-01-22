import System.Environment (getArgs)
import System.IO
import Data.List (foldl')
import qualified Data.Set as Set

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    print $ Set.size $ runMotions $ parseMotions $ lines contents
    --print $ listMotions $ parseMotions $ lines contents

-- Data

data Motion = U | D | R | L deriving (Show)
type Position = (Int, Int)
type PositionSet = Set.Set Position
type Segment = (Position, Position)
type Rope = [Segment]

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

moveKnot :: Motion -> Position -> Position
moveKnot U (x,y) = (x + 1, y)
moveKnot D (x,y) = (x - 1, y)
moveKnot R (x,y) = (x, y + 1)
moveKnot L (x,y) = (x, y - 1)

moves :: Segment -> [Motion]
moves ((xh,yh),(xt,yt))
    | abs dx > 1 = vDir dx ++ hDir dy
    | abs dy > 1 = vDir dx ++ hDir dy
    | otherwise  = []
    where
        dx = xh - xt
        dy = yh - yt
        vDir x | x > 0 = [U] | x < 0  = [D] | otherwise = []
        hDir y | y > 0 = [R] | y < 0 = [L] | otherwise = []

moveSegment :: Segment -> Position
moveSegment r@(_,t) = case moves r of
    [] -> t
    [m] -> moveKnot m t
    [m1, m2] -> moveKnot m2 $ moveKnot m1 t

--runMotions :: [Motion] -> PositionSet
--runMotions xs =
--    let origin = ((0,0),(0,0), Set.empty)
--        moveRope (h, t, s) m = (h', t', s')
--            where
--                h' = moveKnot m h
--                t' = moveSegment (h', t)
--                s' = Set.insert t' s
--        (_, _, set) = foldl' moveRope origin xs
--    in  set

pullRope :: Rope -> Rope
pullRope [] = []
pullRope [r@(h,_)] = [(h, moveSegment r)]
pullRope (r@(h,t):rs) = r' : pullRope rs'
    where t' = moveSegment r
          r' = (h,t')
          (_,t'') = head rs
          rs' = (t',t'') : tail rs

foldRope :: (Rope, PositionSet) -> Motion -> (Rope, PositionSet)
foldRope ((h,t):rs,s) m =
    let h' = moveKnot m h
        rope = pullRope ((h',t):rs)
        s' = Set.insert (snd $ last rope) s
    in  (rope, s')


runMotions :: [Motion] -> PositionSet
runMotions ms =
    let origin = take 9 $ repeat ((0,0),(0,0))
        (_, s) = foldl' foldRope (origin, Set.empty) ms
    in  s

-- Debug

listMotions ms =
    let origin = take 9 $ repeat ((0,0),(0,0))
        (rope, _) = foldl' foldRope (origin, Set.empty) ms
    in  rope
