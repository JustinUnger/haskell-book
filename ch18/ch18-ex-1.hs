import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = Nope deriving (Show, Eq)

instance Functor Nope where
    fmap _ Nope = Nope

instance Applicative Nope where
    pure _ = Nope
    Nope <*> Nope = Nope

instance Monad Nope where
    return = pure
    Nope >>= _ = Nope

instance Arbitrary (Nope a) where
    arbitrary = return Nope

instance EqProp (Nope a) where (=-=) = eq
 
main :: IO ()
main = do 
    let trigger = undefined :: Nope (Int,String,Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
