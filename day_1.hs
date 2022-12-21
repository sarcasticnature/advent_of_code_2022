import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStr $ show $ sum $ take 3 $ reverseSort $ numbers $ removeSpaces contents

backwardsToNums :: [Char] -> [Int]
backwardsToNums = map read . words . reverse

collectNum :: ([Char],[Int]) -> Char -> ([Char],[Int])
collectNum acc x =
    if x == '\n' && (head $ fst acc) == '\n'
    then let nums = backwardsToNums $ fst acc
             total = sum nums
         in ([], total : snd acc)
    else (x : fst acc, snd acc)

numbers :: [Char] -> [Int]
numbers str = let (remain, nums) = foldl collectNum ([],[]) str
              in  reverse $ backwardsToNums remain ++ nums

removeSpaces :: [Char] -> [Char]
removeSpaces = filter (\x -> x /= ' ')

reverseSort :: [Int] -> [Int]
reverseSort [] = []
reverseSort (x:xs) = 
    let lesser = filter (<x) xs
        greater = filter (>x) xs
    in  (reverseSort greater) ++ [x] ++ (reverseSort lesser)

