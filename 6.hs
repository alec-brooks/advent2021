import Data.Map as Map(Map,fromList,empty,foldrWithKey,insert,delete,findWithDefault)
import qualified Data.Map.Strict as Map(insertWith,map)
import Data.List as List(sort,group)

fishGrowth :: Int -> Map Int Int -> Int
fishGrowth 0 fish = sum $ Map.map id fish
fishGrowth i fishNumbers = fishGrowth (i-1) . Map.insertWith (+) 6 nZeros . Map.insertWith (+) 8 nZeros . minusKeys . delete 0 $ fishNumbers
    where nZeros = findWithDefault 0 0 fishNumbers

minusKeys = foldrWithKey (\k v acc  -> insert (k-1) v acc ) empty

countFish :: [Int] -> Map Int Int
countFish = fromList . map (\x -> (head x, length x)) . group . sort

main = do
    let fish = countFish [1,2,1,3,2,1,1,5,1,4,1,2,1,4,3,3,5,1,1,3,5,3,4,5,5,4,3,1,1,4,3,1,5,2,5,2,4,1,1,1,1,1,1,1,4,1,4,4,4,1,4,4,1,4,2,1,1,1,1,3,5,4,3,3,5,4,1,3,1,1,2,1,1,1,4,1,2,5,2,3,1,1,1,2,1,5,1,1,1,4,4,4,1,5,1,2,3,2,2,2,1,1,4,3,1,4,4,2,1,1,5,1,1,1,3,1,2,1,1,1,1,4,5,5,2,3,4,2,1,1,1,2,1,1,5,5,3,5,4,3,1,3,1,1,5,1,1,4,2,1,3,1,1,4,3,1,5,1,1,3,4,2,2,1,1,2,1,1,2,1,3,2,3,1,4,5,1,1,4,3,3,1,1,2,2,1,5,2,1,3,4,5,4,5,5,4,3,1,5,1,1,1,4,4,3,2,5,2,1,4,3,5,1,3,5,1,3,3,1,1,1,2,5,3,1,1,3,1,1,1,2,1,5,1,5,1,3,1,1,5,4,3,3,2,2,1,1,3,4,1,1,1,1,4,1,3,1,5,1,1,3,1,1,1,1,2,2,4,4,4,1,2,5,5,2,2,4,1,1,4,2,1,1,5,1,5,3,5,4,5,3,1,1,1,2,3,1,2,1,1]

    print $ fishGrowth 80 fish
    print $ fishGrowth 256 fish

