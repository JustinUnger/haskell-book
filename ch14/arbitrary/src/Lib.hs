module Lib
     where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

-- must assign a type to identityGen before you can use it
-- the type "a" must also be an instance of Arbitrary

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b =
    Pair a b
    deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a  b =
    First a
  | Second b
  deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b ]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- 10 times more likely to return a First than a Second

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
    a <- arbitrary
    b <- arbitrary
    frequency [(10, return $ First a), (1, return $ Second b)]

-- below sumGenFirstpls is made concrete by assigning concrete types 

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

foo :: IO ()
foo = do
    sample trivialGen
    sample identityGenInt
    sample pairGenIntString

someFunc :: IO ()
someFunc = putStrLn "someFunc"

