count3Increases :: ([Int], Int) ->  Int -> ([Int], Int) 
count3Increases (previous, acc) e = if length previous < 3 then
      (previous ++ [e], acc)
   else do
      let newWindow = tail previous ++ [e]
      if sum newWindow > sum previous then 
         (newWindow, acc + 1)
      else (newWindow, acc)


countIncreases :: (Int, Int) ->  Int -> (Int, Int) 
countIncreases (prev, acc) e = if e > prev then (e, acc + 1) else (e, acc)


main = do
   content <- readFile "1.txt"
   let linesOfFiles = lines content
   let depths = map read linesOfFiles :: [Int]
   let nIncreases  = foldl countIncreases (maxBound::Int, 0) depths
   print $ snd nIncreases

   let n3Increases  = foldl count3Increases ([], 0) depths
   print $ snd n3Increases

