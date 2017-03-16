myWords :: String -> [String]
myWords [] = []
myWords xs = firstWord : myWords $ eat rest -- (dropWhile (== ' ') rest)
    where firstWord = takeWhile (/= ' ') $ eat xs
          rest = dropWhile (/= ' ') $ eat xs
          eat = dropWhile (== ' ')
