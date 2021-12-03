main = do
   content <- readFile "1.txt"
   let linesOfFile = lines content
   let ns = map read linesOfFile :: [Int]

   print $ take 5 ns

