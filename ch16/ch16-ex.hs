{-# LANGUAGE FlexibleInstances #-}
import GHC.Arr
import Test.QuickCheck

-- 1. data Bool = False | true -- can't be a functor because kind is *

-- 2. Yes, kind is * -> *

data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
    fmap f (False' x) = False' (f x)
    fmap f (True' x) = True' (f x)

-- 3. Yes, kind is * -> * 

data BoolAndMaybeSomethingElse a =
    Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish
    fmap f (Truish x) = Truish (f x) 

-- 4. seems like you can't make this a functor?
-- kind of Mu is (* -> *) -> *

newtype Mu f = InF { outF :: f (Mu f) }
newtype Mu' f = InF' (f (Mu' f))

-- instance Functor Mu  where
--    fmap = undefined

-- 5. no, kind of D is *
data D = D (Array Word Word) Int Int

-- rearrach arguments to type constructor 

-- 1. data Sum a b = First a | Second b
data Sum b a = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

-- 2. data Company a b c = DeepBlue a c | Something b
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3. 

data More b a = L a b a
              | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

-- write functor instances for the following

-- 1. 

data Quant a b = 
    Finance
  | Desk a 
  | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)

-- 2. 

data K a b = K a

instance Functor (K a) where
    fmap _ (K x) = K x

-- 3. 

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
    fmap f (Flip (K' x)) = Flip (K' (f x))

-- 4. 

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst (f x)

-- 5. 

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6 

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- 7

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious o a t) = Notorious o a (fmap f t) 

-- 8

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x y) = Cons (f x) (fmap f y)

-- 9 

data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats l m r) = MoreGoats (fmap f l) (fmap f m) (fmap f r)

data TalkToMe a = 
      Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read fs) = Read (f . fs)
