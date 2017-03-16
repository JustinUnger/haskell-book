import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWordTest :: String -> Bool
capitalizeWordTest x = (capitalizeWord x == twice capitalizeWord x) &&
                       (capitalizeWord x == fourTimes capitalizeWord x)

sortIdempotence :: String -> Bool
sortIdempotence x = (sort x == twice sort x) && (sort x == fourTimes sort x)

main :: IO ()
main = do
    quickCheck capitalizeWordTest
    quickCheck sortIdempotence
