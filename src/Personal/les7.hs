wordsCustom :: String -> [(String)]
wordsCustom [] = []
wordsCustom xs = word : wordsCustom rest
    where 
        word = takeWhile(/= ' ') xs
        rest = dropWhile (== ' ') (dropWhile (/= ' ') xs)
    