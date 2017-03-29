import Text.Trifecta
import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative
import Data.List

-- i guess Word64 is as big as we have?
data IPAddress6 = IPAddress6 Word64 Word64
    deriving (Eq, Ord)


foldNibbles :: [Word8] -> Word64
foldNibbles xs
    | length xs > 16 = error "foldNibbles overflow"
    | otherwise = foldl (\acc x -> (fromIntegral x) + (shiftL acc 4)) 0 xs 

foldWords :: [Word16] -> Word64
foldWords xs
    | length xs > 4 = error "foldWords overflow"
    | otherwise = foldl (\acc x -> (fromIntegral x) + (shiftL acc 16)) 0 xs

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
    let h = foldWords (take 4 ws)
        l = foldWords (drop 4 ws)
    return $ IPAddress6 h l

foo :: [Maybe a] -> [a]
foo [] = []
foo (x:xs) = case x of
        Nothing -> []
        Just a  -> a : foo xs
    
hexChars :: [Char]
hexChars = ['0'..'9'] ++ ['a'..'f']

hexValues :: [(Char,Word8)]
hexValues = zip hexChars [0..]

hexValue :: Char -> Word8
hexValue c = fromIntegral $ maybe 0 id (lookup c hexValues)

hexChar :: Word8 -> Char
hexChar x = fst (hexValues !! (fromIntegral x))

showWord16 :: Word16 -> String
showWord16 = padTo '0' 4 . map hexChar . nibblesBE

showAddr6 :: IPAddress6 -> String
showAddr6 = concat . intersperse ":" . map showWord16 . addr6ToWords

addr6ToWords :: IPAddress6 -> [Word16]
addr6ToWords (IPAddress6 h l) = padTo 0 4 highWords ++ padTo 0 4 loWords
    where highWords = word64ToWords16BE h
          loWords   = word64ToWords16BE l

padTo :: a -> Int -> [a] -> [a]
padTo c l xs = go (length xs) xs 
    where go l' xs
            | l' < l = c : go (l'+1) xs
            | otherwise = xs

instance Show IPAddress6 where
    show = showAddr6

-- break 16bit into 4 bit words, little end first
nibblesLE :: Word16 -> [Word8]
nibblesLE 0 = []
nibblesLE w16 = r : nibblesLE q
    where q = w16 `shiftR` 4
          r = fromIntegral $ w16 .&. 0xf

nibblesBE = reverse . nibblesLE

-- break 64bit word into 16 bit words, little end first
word64ToWords16LE :: Word64 -> [Word16]
word64ToWords16LE 0 = []
word64ToWords16LE w64 = r : word64ToWords16LE q
    where q = w64 `shiftR` 16
          r = fromIntegral $ w64 .&. 0xffff

-- big end first
word64ToWords16BE = reverse . word64ToWords16LE

-- use Integer just for test harness
toInteger' :: IPAddress6 -> Integer
toInteger' (IPAddress6 h l) =
    ((fromIntegral h) `shiftL` 64) + fromIntegral l

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

addr6 :: String -> Maybe IPAddress6
addr6 s = case parseString parseAddr6 mempty s of
    Failure _ -> Nothing
    Success r -> Just r

main = do
    putStrLn $ "Passes Parse Tests: " ++ show runV6Tests
