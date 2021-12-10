lBrackets = "([{<"
rBrackets = ")]}>"
bracketPairs = ["()","{}","[]","<>"]

stackOp :: [Char] -> Char -> Maybe [Char]
stackOp stack c 
    | c `elem` lBrackets = Just $ stack ++ [c]
    | c `elem` rBrackets = if (([last stack] ++ [c]) `elem` bracketPairs) then
       Just $ init stack 
       else Nothing
    | otherwise = Just stack

isCorrupted :: [Char] -> [Char] -> Maybe Char
isCorrupted stack [] = Nothing
isCorrupted stack (c:cs) = case (stackOp stack c) of
                            Just s -> isCorrupted s cs
                            Nothing -> Just c

syntaxScores :: Maybe Char -> Int
syntaxScores maybeChar = case maybeChar of 
                 Just ')'  -> 3
                 Just ']'  -> 57
                 Just '}'  -> 1197
                 Just '>'  -> 25137
                 Nothing -> 0
main = do
    content <- readFile "10.txt"
    let linesOfFile = lines content

    print . sum $ map (syntaxScores . isCorrupted []) linesOfFile

