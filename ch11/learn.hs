{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

{-
newtype Goats = Goats Int deriving Show
-}

-- with GeneralizedNewtypeDeriving, we can get this automatically:

newtype Goats = Goats Int deriving (Show, Num)

instance TooMany Goats where
    tooMany (Goats n) = n > 43

{-
-- reuse the original TooMany Int instance we made above: 

instance TooMany Goats where
    tooMany (Goats n) = tooMany n
-}


-- exercises: 

-- make a TooMany instance for (Int, String)

-- with newtype: 
newtype Goats' = Goats' (Int, String) deriving Show

instance TooMany Goats' where
    tooMany (Goats' (n, _)) = n > 43


-- with FlexibleInstances: 

instance TooMany (Int, String) where
    tooMany (n,_) = n > 43


-- Make a TooMany instance for (Int, Int), check sum

--instance TooMany (Int, Int) where
--    tooMany (x,y) = (x+y) > 43


-- make a TooMany instance for (Num a, TooMany a) => (a, a)

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x+y)

