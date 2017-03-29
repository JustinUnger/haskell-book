import Text.Trifecta
import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative
import Data.List

data IPAddress6 = IPAddress6 Word64 Word64
    deriving (Eq, Ord)

toInteger' (IPAddress6 h l) = (toInteger h) * ((toInteger (maxBound :: Word64))+1) + (toInteger l)

foldNibbles :: [Word8] -> Integer
foldNibbles xs = foldl (\acc x -> (fromIntegral x) + (shiftL acc 4)) 0 xs

foldWords :: [Word16] -> Integer
foldWords = foldl (\acc x -> (fromIntegral x) + (shiftL acc 16)) 0

toWord128 :: Integer -> (Word64,Word64)
toWord128 x = let (l,h) = divMod x ((fromIntegral (maxBound :: Word64))+1) in
    (fromIntegral l, fromIntegral h)

parseHexDigit :: Parser Char
parseHexDigit = oneOf hexChars <|> oneOf (toUpper <$> hexChars)

parseNibble :: Parser Word8
parseNibble = do
    c <- toLower <$> parseHexDigit <?> "invalid hex digit"
    return $ hexValue c

parseWord16 :: Parser Word16
parseWord16 = do
    -- take up to four optional nibbles
    -- maybe char
    mc1 <- optional parseNibble
    mc2 <- optional parseNibble
    mc3 <- optional parseNibble
    mc4 <- optional parseNibble
    let xs = foo [mc1,mc2,mc3,mc4]
    return $ fromIntegral $ foldNibbles xs

padWords :: Int -> [Word16] -> [Word16]
padWords 0 xs = xs
padWords _ [] = []
padWords n (x:xs) = if x == 0 then replicate (n+1) 0 ++ xs else x : padWords n xs
    

parseWords :: Parser [Word16]
parseWords = do
    words <- parseWord16 `sepBy` char ':'
    let padBy = 8 - length words
    return $ padWords padBy words

parseAddr6 :: Parser IPAddress6
parseAddr6 = do
    ws <- parseWords
    return $ uncurry IPAddress6 $ toWord128 $ foldWords ws

foo :: [Maybe a] -> [a]
foo [] = []
foo (x:xs) = case x of
        Nothing -> []
        Just a  -> a : foo xs
    
hexChars :: [Char]
hexChars = ['0'..'9'] ++ ['a'..'f']

hexValues :: [(Char,Int)]
hexValues = zip hexChars [0..]

hexValue :: Char -> Word8
hexValue c = fromIntegral $ maybe 0 id (lookup c hexValues)

--hexChar :: Integer -> Char
hexChar x = fst (hexValues !! (fromIntegral x))

words16 :: Integral a => a -> [a]
words16 = baseDigits (2^16)

showWord16 :: Integer -> String
showWord16 = padTo '0' 4 . map hexChar . nibbles

showAddr6 :: IPAddress6 -> String
showAddr6 = concat . intersperse ":" . map showWord16 . words16 . toInteger'

showWord16' :: Integral a => a -> String
showWord16' = padTo '0' 4 . map hexChar . nibbles

showAddr6' :: IPAddress6 -> String
showAddr6' = concat . intersperse ":" . map showWord16' . addr6ToWords

addr6ToWords :: IPAddress6 -> [Word64]
addr6ToWords (IPAddress6 h l) = padTo 0 4 (words16 h) ++ padTo 0 4 (words16 l)

--padTo :: Char -> Int -> String -> String
padTo :: a -> Int -> [a] -> [a]
padTo c l xs = go (length xs) xs 
    where go l' xs
            | l' < l = c : go (l'+1) xs
            | otherwise = xs

instance Show IPAddress6 where
    show = showAddr6

--nibbles :: Integer -> [Integer]
nibbles :: Integral a => a -> [a]
nibbles = baseDigits (2^4)

--baseDigits :: Integer -> Integer -> [Integer]
baseDigits _ 0 = []
baseDigits b n =
    let (q,r) = divMod n b in
        baseDigits b q ++ [r]

testV6Parse (s,i) = case parseString (toInteger' <$> parseAddr6) mempty s of
    Failure _ -> False
    Success i' -> i' == i

v6tests = [
       ("0:0:0:0:0:ffff:ac10:fe01", 0xffffac10fe01)
    ,  ("0:0:0:0:0:ffff:cc78:f", 0xffffcc78000f)
    ,  ("FE80:0000:0000:0000:0202:B3FF:FE1E:8329", 0xfe800000000000000202b3fffe1e8329)
    ,  ("FE80::::0202:B3FF:FE1E:8329", 0xfe800000000000000202b3fffe1e8329)
    ,  ("FE80::0202:B3FF:FE1E:8329", 0xfe800000000000000202b3fffe1e8329)
    ,  ("2001:0DB8:0000:0000:0008:0800:200C:417A", 0x20010db80000000000080800200c417a)
    ,  ("2001:DB8::8:800:200C:417A", 0x20010db80000000000080800200c417a) ]


runV6Tests = all testV6Parse v6tests

main = do
    putStrLn $ "Passes Parse Tests: " ++ show runV6Tests
