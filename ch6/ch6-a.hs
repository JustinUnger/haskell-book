import Data.List

-- 1

i :: Num a => a
-- i :: a
i = 1

-- can't use i :: a because it's is more generic than Num a => a

-- 2

f :: Float
-- f :: Num a => a
f = 1.0

-- can't use f :: Num a => a because it is more generic than Float
-- Num is only for whole numbers

-- 3

-- f' :: Float
f' :: Fractional a => a
f' = 1.0

-- this works because 1.0 is a Fractional

-- 4

-- f'' :: Float
f'' :: RealFrac a => a
f'' = 1.0

-- this should work because Float and Double have instances of RealFrac

-- 5

-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- this will work, but is an unneccessary constraint because function doesn't do 
-- anything with the data
-- GHC generates warning when -Wall is used

-- 6

-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- this works because it goes more concrete, but is still unnecessary

-- 7

myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund _ = myX

-- this doesn't work because function discards argument and always returns an int,
-- which could be a different type than passed

-- 8

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' _ = myX

-- this should work because myX is an Int which has an instance of Num
-- ^ wrong answer, this doesn't work for same reason as #7

-- 9

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- this works because it goes more concrete and Int has instance of Ord

-- 10

--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- this works because sort only requires an Ord instance  

-- 11

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

-- don't think this will work because mySort requires a [Char], 
-- can't force signifier to be more generic
