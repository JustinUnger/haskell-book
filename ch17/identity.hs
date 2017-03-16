newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity x) = Identity (f x)


newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant x) (Constant y) = Constant (x `mappend` y)


-- exercise fixer upper

-- 1

--let x = const <$> Just "Hello" <*> "World"
x = const <$> Just "Hello" <*> pure "World"

-- 2. 
-- (,,,) Just 90 <*> Justin 10 Just "Tierness" [1,2,3]

x' = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]




