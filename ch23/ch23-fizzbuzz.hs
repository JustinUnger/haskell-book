import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list =
    let dlist = execState (mapM_ addResult' list) DL.empty
    in DL.apply dlist []

fizzbuzzFromTo :: Integer -> Integer -> [String] 
fizzbuzzFromTo from to
	| from > to  = []
    | otherwise = fizzBuzz from : (fizzbuzzFromTo (from + 1) to)    

fizzbuzzList'' :: [Integer] -> DL.DList String
fizzbuzzList'' list =
    execState (mapM_ addResult' list) DL.empty

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result :xs)

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)

main :: IO ()
main =
    mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

main' :: IO ()
main' = mapM_ (putStrLn . fizzBuzz) [1..100]

