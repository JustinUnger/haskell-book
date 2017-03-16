import Text.Trifecta
import Data.Char

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
    digits <- some parseDigit
    return $ foldl (\acc x -> (fromIntegral $ digitToInt x) + (acc * 10)) 0 digits

base10Integer' :: Parser Integer
-- handle negative numbers also
base10Integer' = do
    sign <- optional (oneOf "+-")
    x <- base10Integer
    return $ maybe x (\s -> s == '-' ? negate x $ x) sign
{-
    case sign of 
        Just s -> if s == '-' then return (negate x) else return x
        Nothing -> return x
-}

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'


