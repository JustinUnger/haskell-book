import Data.Char

notThe :: String -> Maybe String
notThe xs = case xs of
    "the" -> Nothing
    x     -> Just x

replaceThe :: String -> String
replaceThe xs = unwords $ map foo $ map notThe $ words xs

foo x = case x of
    Nothing -> "a"
    Just x  -> x

countTheBeforeVowel xs = go $ words xs


go :: [String] -> Integer
go []  = 0
go [_] = 0
go (x:y:ys) = case x of 
                "the" -> if vowelFirst y then 1 + go (y:ys) else go (y:ys)
                otherwise  -> go (y:ys)

vowelFirst (x:xs) = if x `elem` "aeiou" then True else False

countVowels xs = length $ filter (== True) $ map (\x -> x `elem` "aeiou") xs
countVowels' :: String -> Integer
countVowels' = foldr (\x acc -> if x `elem` "aeiou" then acc + 1 else acc) 0

newtype Word' = Word' String deriving (Show, Eq)

mkWord :: String -> Maybe Word'
mkWord xs = if vowels > non then Nothing else Just (Word' xs)
    where vowels = length $ filter (flip elem "aeiou") xs
          non = (length xs) - vowels

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

helper :: Integer -> Nat
helper 0 = Zero
helper x = Succ $ helper (x-1)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just (helper x)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing = z
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing = z
fromMaybe z (Just x) = x

fromMaybe' :: a -> Maybe a -> a
fromMaybe' z m = mayybee z id m

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of 
                    Nothing -> catMaybes xs
                    Just x -> x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
    | anyNothings  = Nothing
    | otherwise    = Just (catMaybes xs)
    where anyNothings = foldr (\x acc -> case x of 
                                           Nothing -> True
                                           Just _ -> acc) False xs

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (x:xs) = case x of 
                  Left x -> x : lefts' xs
                  Right _ -> lefts' xs

lefts :: [Either a b] -> [a]
lefts = foldr (\x acc -> case x of
                            Left x -> x : acc
                            Right _ -> acc) []

rights :: [Either a b] -> [b]
rights = foldr (\x acc -> case x of 
                            Left _ -> acc
                            Right x -> x : acc) []

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = 
    foldr (\x (acca,accb) -> 
              case x of
                Left x -> (x : acca, accb)
                Right x -> (acca, x : accb)) ([],[])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f m = either' (\_ -> Nothing) (\x -> Just (f x)) m

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f z = case f z of 
                  Nothing -> []
                  Just (x,y) -> x : unfoldr' f y

iterate'' :: (a -> a) -> a -> [a]
iterate'' f z = unfoldr' (\x -> Just (x, f x)) z

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> Tree b
unfold f z = 
    case f z of
        Nothing -> Leaf
        Just (l,v,r) -> Node (unfold f l) v (unfold f r)

treeBuild :: Integer -> Tree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just (x+1,x,x+1)) 0 


