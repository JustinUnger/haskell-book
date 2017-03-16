import Data.Char

filterCaps :: String -> String
filterCaps [] = []
filterCaps (x:xs) = if isUpper x then x : filterCaps xs else filterCaps xs

capFirst :: String -> String
capFirst [] = []
capFirst (x:xs) = toUpper x : xs

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs

firstLetter :: String -> String
firstLetter [] = []
firstLetter xs = [toUpper $ head xs]

firstLetter' :: String -> String
firstLetter' xs = [toUpper . head $ xs]

firstLetter'' = flip (:) [] . toUpper . head 

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if e == x then True else myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs = any (== e) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x:y:ys) = if f x y == GT then myMaximumBy f (x:ys) else myMaximumBy f (y:ys)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x:y:ys) = if f x y == LT then myMinimumBy f (x:ys) else myMinimumBy f (y:ys)

myMaximum :: Ord a => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: Ord a => [a] -> a
myMinimum xs = myMinimumBy compare xs
