import Test.QuickCheck
import Data.List (sort)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

propOrdered :: [Int] -> Bool
propOrdered = listOrdered . sort

plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative x y =
    x + y == y + x

multAssociative x y z = 
    x * (y * z) == (x * y) * z

multCommutative x y =
    x * y == y * x

propAssociative :: Int -> Int -> Int -> Bool
propAssociative = plusAssociative

propCommutative :: Int -> Int -> Bool
propCommutative = plusCommutative


propMultAssociativeInt :: Int -> Int -> Int -> Bool
propMultAssociativeInt = multAssociative

propMultCommutativeInt :: Int -> Int -> Bool
propMultCommutativeInt = multCommutative

propQuotRem :: Integer -> Integer -> Bool
propQuotRem x y
    | y == 0 = True
    | otherwise = (quot x y) * y + (rem x y) == x

propDivMod :: Integer -> Integer -> Bool
propDivMod x y
    | y == 0 = True
    | otherwise = (div x y) * y + (mod x y) == x

propQuotRem' :: NonZero Integer -> NonZero Integer -> Bool
propQuotRem' (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x

propDivMod' :: NonZero Integer -> NonZero Integer -> Bool
propDivMod' (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x

powAssoc :: Integer -> Integer -> Integer -> Bool
powAssoc x y z = (x ^ y) ^ z == x ^ (y ^ z)

powComm :: Integer -> Integer -> Bool
powComm x y = x ^ y == y ^ x

reverseId :: String -> Bool
reverseId x  = reverse (reverse x) == x

concatProp :: String -> String -> Bool
concatProp x y = foldr (:) x y == (++) x y

concatProp' :: [String] -> Bool
concatProp' x = foldr (++) [] x == concat x

fProp :: Int -> String -> Bool
fProp n xs = length (take n xs) == n

readShowProp :: Double -> Bool
readShowProp x = (read (show x)) == x

square x = x * x
squareIdentity = square . sqrt

squareProp :: Double -> Bool
squareProp x = squareIdentity x == x

main :: IO ()
main = do
    quickCheck propOrdered
    quickCheck propAssociative
    quickCheck propCommutative
    quickCheck propMultAssociativeInt
    quickCheck propMultCommutativeInt
    quickCheck propQuotRem 
    quickCheck propDivMod
    quickCheck propQuotRem'
    quickCheck propDivMod'
-- exponent operator is not associative or commutative, these will fail
    quickCheck powAssoc
    quickCheck powComm

    quickCheck reverseId
    quickCheck concatProp
    quickCheck concatProp'
    quickCheck fProp 
    quickCheck readShowProp
    quickCheck squareProp
