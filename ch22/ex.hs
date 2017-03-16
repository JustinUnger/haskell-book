import Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' = do
    c <- cap
    r <- rev
    return (c,r)

tupled'' :: String -> (String, String)
tupled'' = cap >>= 
                \c -> rev >>=
                       \r -> return (c,r)
