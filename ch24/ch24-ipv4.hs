import Data.Char
import Data.Word
import Data.Bits
import Text.Trifecta
import Data.List

data IPAddress = IPAddress Word32 deriving (Eq, Ord)

parseByte :: Parser Word8
parseByte = do
    x <- natural
    if x > 255 then fail "byte out of range" else return (fromIntegral x)
    
parseIPv4 :: Parser IPAddress
parseIPv4 = do
    b1 <- parseByte
    char '.'
    b2 <- parseByte
    char '.'
    b3 <- parseByte
    char '.'
    b4 <- parseByte
    return $ foldAddr [b1,b2,b3,b4]

foldAddr :: [Word8] -> IPAddress
foldAddr xs = IPAddress $ foldl (\acc x -> (fromIntegral x) + (shiftL acc 8)) 0 xs

test1 = parseString parseIPv4 mempty "172.16.254.1"
test2 = parseString parseIPv4 mempty "204.120.0.15"

instance Show IPAddress where
    show (IPAddress x) = concat $ intersperse "." $ map show bytes
        where bytes = baseDigits 256 x

baseDigits _ 0 = []
baseDigits b n =
    let (q,r) = divMod n b in
        baseDigits b q ++ [r]
