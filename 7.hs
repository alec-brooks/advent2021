import Data.List.Split

part1 :: [Int] -> Int -> Int
part1 subs i = sum $ map (abs . subtract i) subs

sumOfFirstNumbers :: Int -> Int
sumOfFirstNumbers n = quot (n * (n+1)) 2

part2 :: [Int] -> Int -> Int
part2 subs i = sum $ map (sumOfFirstNumbers . abs . subtract i) subs

fuelExpenditure :: [Int] -> (Int -> Int) -> Int -> Int
fuelExpenditure subs fuelF i = sum $ map (fuelF . abs . subtract i) subs

main = do
    content <- readFile "7.txt"
    let subs = map read $ splitOn "," content ::[Int]
    let subRange = [(minimum subs) .. (maximum subs)]

    print $ minimum $ map (fuelExpenditure subs id) subRange
    print $ minimum $ map (fuelExpenditure subs sumOfFirstNumbers) subRange

