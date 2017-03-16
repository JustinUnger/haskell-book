import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char],[Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char],[Char])
tupled' = do
    c <- cap
    r <- rev
    return (c,r)

tupled'' :: [Char] -> ([Char],[Char])
tupled'' =
    cap >>= (\c -> rev >>= (\r -> return (c,r)))
