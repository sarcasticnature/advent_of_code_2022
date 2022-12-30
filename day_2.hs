import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStr $ show $ tallyGames $ splitGames contents

data Choice = Rock | Paper | Scissors deriving (Eq, Show)

instance Ord Choice where
    compare Rock x = case x of
        Rock     -> EQ
        Paper    -> LT
        Scissors -> GT
    compare Paper x = case x of
        Rock     -> GT
        Paper    -> EQ
        Scissors -> LT
    compare Scissors x = case x of
        Rock     -> LT
        Paper    -> GT
        Scissors -> EQ

splitGames :: String -> [String]
splitGames [] = []
splitGames str = let (x, xs:xs') = span (/= '\n') str
                 in  x : splitGames xs'     -- need to get rid of the leading newline

-- TODO: obviously brittle
decodeChoice :: Char -> Choice
decodeChoice char
    | char == 'A' || char == 'X' = Rock
    | char == 'B' || char == 'Y' = Paper
    | char == 'C' || char == 'Z' = Scissors
    | otherwise                  = error $ "Could not decode char: " ++ show char

choicePoints :: Choice -> Int
choicePoints choice
    | choice == Rock     = 1
    | choice == Paper    = 2
    | choice == Scissors = 3

winPoints :: Ordering -> Int
winPoints order
    | order == LT = 0
    | order == EQ = 3
    | order == GT = 6

decodeOrder :: Char -> Ordering
decodeOrder char
    | char == 'X' = LT
    | char == 'Y' = EQ
    | char == 'Z' = GT

pickChoice :: Choice -> Ordering -> Choice
pickChoice choice rightOrder
    | rightOrder == rockOrder = Rock
    | rightOrder == paperOrder = Paper
    | rightOrder == scissorsOrder = Scissors
    where
        rockOrder = compare Rock choice
        paperOrder = compare Paper choice
        scissorsOrder = compare Scissors choice

scoreGame :: Int -> String -> Int
scoreGame acc str =
    let opponent = decodeChoice $ head str
        order = decodeOrder $ last str
        self = pickChoice opponent order
        choicePts = choicePoints self
        winPts = winPoints order
    in  acc + choicePts + winPts

tallyGames :: [String] -> Int
tallyGames games = foldl scoreGame 0 games

