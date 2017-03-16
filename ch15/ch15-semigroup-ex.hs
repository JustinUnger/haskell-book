import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

type IdentityAssocString = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x x') <> (Two y y') = Two (x <> y) (x' <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

type TwoAssocStringInt = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x x' x'') <> (Three y y' y'') = Three (x <> y) (x' <> y') (x'' <> y'')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
    Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

type ThreeAssocStringIntDouble = Three String [Int] [Double] -> Three String [Int] [Double] -> Three String [Int] [Double] -> Bool

-- data Four a b c d = Four a b c d is same as above just more

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

-- > (BoolConj True) <> (BoolConj True)
-- BoolConj True
-- > (BoolConf True) <> (BoolConj False)
-- BoolConj False

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
    arbitrary = do 
        x <- arbitrary
        return (BoolConj x)

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

-- conjunction is logical AND
-- what it should do: 
-- > (BoolDisj True) <> (BoolDisj True) 
-- BoolDisj True
-- > (BoolDisj True) <> (BoolDisj False)
-- BoolDisj True

instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return (BoolDisj x)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd x) <> _       = Snd x
    _       <> (Snd x) = Snd x
    _ <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        --oneof [return (Fst x), return (Snd y)]
        elements [Fst x, Snd y]

type OrAssocStringInt = Or String Int -> Or String Int -> Or String Int -> Bool

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (f <> g)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
    (Comp x) <> (Comp y) = Comp (x <> y)

data Validation a b =
    Failure' a | Success' b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure' x) <> (Failure' y) = Failure' (x <> y)
    (Failure' x) <> _ = Failure' x
    _ <> (Failure' x) = Failure' x
    x <> _            = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Failure' x, Success' y]

type ValidationAssocStringInt = Validation String Int -> Validation String Int -> Validation String Int -> Bool

newtype AccumulateRight a b =
    AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success' x)) <> (AccumulateRight (Success' y)) = (AccumulateRight (Success' (x <> y)))
    (AccumulateRight (Failure' x)) <> _ = (AccumulateRight (Failure' x))
    _ <> (AccumulateRight (Failure' x)) = (AccumulateRight (Failure' x))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [ AccumulateRight (Failure' x), AccumulateRight (Success' y) ]

type AccumulateRightAssocIntOrdering = AccumulateRight Int Ordering -> AccumulateRight Int Ordering -> AccumulateRight Int Ordering -> Bool

newtype AccumulateBoth a b =
    AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => 
    Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success' x)) <> (AccumulateBoth (Success' y)) = (AccumulateBoth (Success' (x <> y)))
    (AccumulateBoth (Failure' x)) <> (AccumulateBoth (Failure' y)) = (AccumulateBoth (Failure' (x <> y)))
    (AccumulateBoth (Failure' x)) <> _ = (AccumulateBoth (Failure' x))
    _ <> (AccumulateBoth (Failure' x)) = (AccumulateBoth (Failure' x))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [ AccumulateBoth (Failure' x), AccumulateBoth (Success' y) ] 

type AccumulateBothAssocIntOrdering = AccumulateBoth [Int] Ordering -> AccumulateBoth [Int] Ordering -> AccumulateBoth [Int] Ordering -> Bool
        

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentityAssocString)
    quickCheck (semigroupAssoc :: TwoAssocStringInt)
    quickCheck (semigroupAssoc :: ThreeAssocStringIntDouble)
    quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssocStringInt)
    quickCheck (semigroupAssoc :: ValidationAssocStringInt)
    quickCheck (semigroupAssoc :: AccumulateRightAssocIntOrdering)
    quickCheck (semigroupAssoc :: AccumulateBothAssocIntOrdering)
