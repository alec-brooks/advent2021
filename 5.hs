import Data.List
import Data.List.Split
import qualified Data.Map as Map(Map,empty,insertWith,filter,findWithDefault)

horVerticalFilter :: [(Int,Int)] -> Bool
horVerticalFilter [start,end] = (fst start == fst end) || (snd start == snd end)

countLines :: Map.Map (Int, Int) Int -> (Int, Int) -> Map.Map (Int, Int) Int 
countLines acc pos = Map.insertWith (+) pos 1 acc

range' :: Int -> Int -> [Int]
range' a b = if a > b then [b..a] else [a..b]

range2 :: [(Int, Int)] -> [(Int, Int)]
range2 [(x1,y1),(x2,y2)]
    | x1 == x2           = zip (cycle [x1]) (range' y1 y2)
    | y1 == y2           = zip (range' x1 x2) (cycle [y1])
    | x1 < x2 && y1 < y2 = zip [x1..x2] [y1..y2]
    | x1 > x2 && y1 < y2 = zip (reverse [x2..x1]) [y1..y2]
    | x1 > x2 && y1 > y2 = zip (reverse [x2..x1]) (reverse [y2..y1])
    | x1 < x2 && y1 > y2 = zip [x1..x2] (reverse [y2..y1])

tuplify :: [Int] -> (Int,Int)
tuplify [a,b] = (a,b)

nOverlaps :: [[(Int,Int)]] -> Int
nOverlaps lines = length . Map.filter (> 1) . foldl countLines Map.empty . concat $ map range2 lines

main = do
    content <- readFile "5.txt"
    let linesOfFile = lines content
    let linesCoords = map (map (tuplify. (map read) . splitOn ",") .  splitOn " -> ") linesOfFile 
    let part1Lines = filter horVerticalFilter linesCoords

    print $ nOverlaps part1Lines
    print $ nOverlaps linesCoords
