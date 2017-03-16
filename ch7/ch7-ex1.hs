eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop = go (fromEnum start) (fromEnum stop)
    where go x y 
           | x > y = []
           | otherwise = (toEnum x) : go (x+1) y

eftInt :: Int -> Int -> [Int]
eftInt start stop = go (fromEnum start) (fromEnum stop)
    where go x y 
           | x > y = []
           | otherwise = (toEnum x) : go (x+1) y

eftChar :: Char -> Char -> [Char]
eftChar start stop = go (fromEnum start) (fromEnum stop)
    where go x y 
           | x > y = []
           | otherwise = (toEnum x) : go (x+1) y
