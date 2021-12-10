import Data.List.Split
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

tuplify [a,b] = (a,b)

part1 :: [Int] -> [String] -> [String] ->  Int
part1 ul _ output = length $ filter ((`elem` ul) . length) output

unsafeFind p = maybe "" id . find p

getCodeOfLength :: Int -> [String] -> String
getCodeOfLength l = unsafeFind((== l) . length)

part2 :: [String] -> [String] -> Int 
part2 codes output = do
    let c = map sort codes
    let one = getCodeOfLength 2 c
    let four = getCodeOfLength 4 c
    let seven = getCodeOfLength 3 c
    let six = unsafeFind (\x -> ((== 6) . length) x && not (isSubsequenceOf seven x)) c
    let eight = getCodeOfLength 7 c
    let nine = unsafeFind (\x -> ((== 6) . length) x && isSubsequenceOf four x) c
    let zero = unsafeFind (\x -> ((== 6) . length) x && x /= nine && x /= six) c
    let three = unsafeFind (\x -> ((== 5) . length) x && isSubsequenceOf seven x) c
    let five = unsafeFind (\x -> ((== 5) . length) x && not ((S.findMin $ S.difference (S.fromList eight) (S.fromList six)) `elem` x)) c
    let two = unsafeFind (\x -> ((== 5) . length) x && x /= five && x /= three) c

    let mappedCodes = M.fromList $ zip [zero,one,two,three,four,five,six,seven,eight,nine] (map show [0..])

    read $ concat $ map (\x -> mappedCodes M.! (sort x)) output

main = do
    content <- readFile "8.txt"
    let linesOfFile = lines content
    let codesOutput = map (tuplify . map words . splitOn "|") linesOfFile

    let uniqueLengths = [2,4,3,7]

    print $ sum $ map (uncurry (part1 uniqueLengths)) codesOutput
    print $ sum $ map (uncurry part2) codesOutput


