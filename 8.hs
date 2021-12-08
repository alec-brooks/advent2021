import Data.List.Split
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

tuplify [a,b] = (a,b)

part1 :: [Int] -> [String] -> [String] ->  Int
part1 ul _ output = length $ filter ((`elem` ul) . length) output

getCodeOfLength :: Int -> [String] -> String
getCodeOfLength l = maybe "" id . find ((== l) . length)

part2 :: [String] -> [String] -> Int 
part2 codes output = do
    let c = map sort codes
    let one = getCodeOfLength 2 c
    let four = getCodeOfLength 4 c
    let seven = getCodeOfLength 3 c
    let six = maybe "" id $ find (\x -> ((== 6) . length) x && not (isSubsequenceOf seven x)) c
    let eight = getCodeOfLength 7 c
    let nine = maybe "" id $ find (\x -> ((== 6) . length) x && isSubsequenceOf four x) c
    let zero = maybe "" id $ find (\x -> ((== 6) . length) x && x /= nine && x /= six) c

    let segmentMapping = M.fromList 
                       [ ("a", S.findMin $ S.difference (S.fromList seven) (S.fromList one))
                       , ("c", S.findMin $ S.difference (S.fromList eight) (S.fromList six))
                       , ("d", S.findMin $ S.difference (S.fromList eight) (S.fromList zero))
                       , ("e", S.findMin $ S.difference (S.fromList eight) (S.fromList nine)) ]

    let three = maybe "" id $ find (\x -> ((== 5) . length) x && isSubsequenceOf seven x) c
    let five = maybe "" id $ find (\x -> ((== 5) . length) x && not (segmentMapping M.! "c" `elem` x)) c
    let two = maybe "" id $ find (\x -> ((== 5) . length) x && x /= five && x /= three) c

    let updatedSegment = M.union segmentMapping (M.fromList 
            [ ("b", S.findMin $ S.difference (S.fromList eight) (S.union (S.fromList two) (S.fromList one)))])
                        
    let mappedCodes = M.fromList $ zip [zero,one,two,three,four,five,six,seven,eight,nine] (map show [0..])

    read $ concat $ map (\x -> mappedCodes M.! (sort x)) output

main = do
    content <- readFile "8.txt"
    let linesOfFile = lines content
    let codesOutput = map (tuplify . map words . splitOn "|") linesOfFile

    let correctCodes = ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"]

    let uniqueLengths = [2,4,3,7]

    print $ sum $ map (uncurry (part1 uniqueLengths)) codesOutput
    print $ sum $ map (uncurry part2) codesOutput


