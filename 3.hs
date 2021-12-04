import Data.List
import Data.Bool(bool)

parseStringToBools :: String -> [Bool]
parseStringToBools s = do 
    let numbers = map (read . (:"")) s:: [Int]
    map (== 1) numbers

bintodec :: (Foldable f, Integral i) => f Bool -> i
bintodec = foldl (\a -> (+) (2*a) . bool 0 1) 0

oxFilter :: [[Bool]] -> Int -> [Bool] -> Bool
oxFilter c i r = case maybeCommonNumber $ c!!i of 
                   Just x -> r!!i == x
                   Nothing -> r!!i

co2Filter :: [[Bool]] -> Int -> [Bool] -> Bool
co2Filter c i r = case maybeCommonNumber $ c!!i of 
                   Just x -> r!!i == not x
                   Nothing -> not $ r!!i

rating :: ([[Bool]] -> Int -> [Bool] -> Bool) -> Int -> [[Bool]] -> [Bool]
rating _ _ [] = []
rating _ _ [r] = r
rating f i rs = rating f (i + 1) $ filter (f (transpose rs) i) rs 

commonNumber :: [Bool] -> Bool
commonNumber c = (> quot (length c) 2) . sum . map fromEnum $ c

maybeCommonNumber :: [Bool] -> Maybe Bool
maybeCommonNumber c = do
    let halfway = quot (length c) 2 + (bool 0 1 (mod (length c) 2 == 1))
    let oneCount = sum . map fromEnum $ c
    let result | (oneCount > halfway) = Just True
               | (oneCount < halfway) = Just False
               | otherwise = Nothing
    result

main = do
    content <- readFile "3.txt"
    let linesOfFile = map parseStringToBools $ lines content
    let columns = transpose linesOfFile

    let gamma = map commonNumber columns
    let epsilon = map not gamma
    print $ (bintodec gamma) * (bintodec epsilon)
    print $ (bintodec $ rating (oxFilter) 0 linesOfFile) * (bintodec $ rating (co2Filter) 0 linesOfFile)

