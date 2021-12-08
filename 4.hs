import Data.List
import Data.List.Split

parseBoards  :: [String] -> [[[String]]]
parseBoards lines = map (map words) $ splitOn [""] $ tail $ tail lines

checkBoardIsWinner :: [String] -> [[String]] -> Bool
checkBoardIsWinner [] _ = False
checkBoardIsWinner draws board = any (all (`elem` draws)) $ board ++ transpose board

whenBoardWins :: [String] -> [[String]] -> Int
whenBoardWins draws board = if checkBoardIsWinner draws board then
                                whenBoardWins (init draws) board
                            else 
                                length draws

scoreBoard :: [String] -> [[String]] -> Int -> Int
scoreBoard draws board nDraws = sumOfUnmarked * finalNumber 
    where sumOfUnmarked = (sum . map read . filter (`notElem`  take (nDraws+1) draws) . concat) board 
          finalNumber = read $ draws !! nDraws

main = do
    content <- readFile "4.txt"
    let linesOfFile = lines content
    let draws = splitOn "," $ head linesOfFile
    let boards = parseBoards linesOfFile
    let boardsByScore = sortBy (\(_,a) (_,b) -> compare a b) $ zip boards $ map (whenBoardWins draws) boards
    let winningBoard = head boardsByScore
    let losingBoard = last boardsByScore

    print $ uncurry (scoreBoard draws) winningBoard
    print $ uncurry (scoreBoard draws) losingBoard

