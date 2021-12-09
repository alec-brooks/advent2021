import Data.Char
import Data.List
import Debug.Trace

isSmaller :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
isSmaller heights x1 y1 x2 y2 = heights!!x1!!y1 < heights!!x2!!y2

isXYSmaller heights x y = uncurry $ isSmaller heights x y

isLowPoint :: [[Int]] -> Int -> Int -> Bool
isLowPoint heights x y = checkNeighbours (isXYSmaller heights x y) heights x y

checkNeighbours :: ((Int, Int) -> Bool) -> [[Int]] -> Int -> Int -> Bool
checkNeighbours f heights x y 
    | x == 0 && y == 0    = all f [(0,1),(1,0)]
    | x == 0 && not maxY  = all f [(0,y-1),(0,y+1),(1,y)]
    | x == 0 && maxY      = all f [(0,y-1),(1,y)]
    | y == 0 && not maxX  = all f [(x-1,0),(x+1,0),(x,1)]
    | y == 0 && maxX      = all f [(x-1,0),(x,1)]
    | maxX && not (y==0)  = all f [(x-1,y),(x,y-1),(x,y+1)]
    | maxY && not (x==0)  = all f [(x-1,y),(x,y-1),(x+1,y)]
    | maxX && maxY        = all f [(x,y-1),(x-1,y)]
    | otherwise           = all f [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]
  where 
        maxX = x >= (length heights)-1
        maxY = y >= (length $ head heights)-1

main = do
    content <- readFile "9.s.txt"
    let linesOfFile = lines content
    let heights = map (map digitToInt) linesOfFile :: [[Int]]

    let is = concat $ map (\x -> map (\y -> (x,y)) [0..(length $ head heights)-1]) [0..(length heights)-1]

    let lowPointCoords = filter (uncurry (isLowPoint heights)) is
    print . sum . map (+1) $ map (\(x,y) -> heights!!x!!y) lowPointCoords
    --print $  (isLowPoint heights) 

