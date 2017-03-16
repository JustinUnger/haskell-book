import Data.Char

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node l v r)
    | x == v    = Node l v r
    | x < v     = Node (insert' x l) v r
    | otherwise = Node l v (insert' x r)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l v r) =
    Node (mapTree f l) (f v) (mapTree f r)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l v r) = v : (preorder l) ++ (preorder r)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l v r) = (inorder l) ++ [v] ++ (inorder r)

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node l v r) = (postOrder l) ++ (postOrder r) ++ [v]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node l v r) = foldTree f (foldTree f (f v z) l) r

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf _ [] = False
isSubsequenceOf [] _ = True
isSubsequenceOf a@(x:xs) b@(y:ys)
    | x == y = isSubsequenceOf xs b
    | otherwise = isSubsequenceOf a ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = zip (words xs) $ map capitalizeFirst $ words xs
    where capitalizeFirst (x:xs) = toUpper x : xs

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

type Sentence = [String]
type Paragraph = [Sentence]
capitalizeParagraph xs = words xs  

w = words "hey you whats that sound. there. we are there"

isLastWord w = if (last w) == '.' then True else False

{-
sentence xs = (takeWhile (not . isLastWord) xs ++ [head $ dropWhile (not . isLastWord) xs], rest xs) 
    where rest [] = [] 
          rest xs = tail $ dropWhile (not . isLastWord) xs
-}

takeSentence [] = []
takeSentence [x] = if isLastWord x then [x] else []
takeSentence (x:xs) = if isLastWord x then [x] else x : takeSentence xs

dropSentence [] = []
dropSentence [x] = []
dropSentence (x:xs) = if isLastWord x then xs else dropSentence xs

f (x,[]) = (x,[])
f (sentences,rest) = f (sentences ++ [takeSentence rest], dropSentence rest)

g (s,[]) = s

data Keypad = Keypad
type Digit = Char
type Presses = Int

reverseTaps :: Keypad -> Char -> [(Digit, Presses)]
reverseTaps (ButtonOne s) c
    | c `elem` s = press '1' s c

press :: Digit -> String -> Char -> [(Digit, Presses)]
press d xs c
    | isUpper c = [('*',1),(d,indexOf c xs)]
    | otherwise = [(d,indexOf c xs)] 
