{-# LANGUAGE InstanceSigs #-}

import Control.Monad (join)

newtype Identity a =
    Identity { runIdentity :: a } deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

newtype IdentityT f a = 
    IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)

    (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
    return = pure

    (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a
          -> (a -> IdentityT m b)
          -> IdentityT m b

    (IdentityT ma) >>= f = -- join (fmap runIdentityT (fmap f ma))
        --let aimb = ma >>= f in undefined
        let aimb = join (fmap runIdentityT (fmap f ma)) in IdentityT aimb

  --(IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

f :: a -> IdentityT m b
f = undefined

cf :: a -> m b
cf = runIdentityT . f

ex1 :: IdentityT [] Integer
ex1 = IdentityT [1,2,3] >>= (return . (+1))

ex2 :: IdentityT Maybe Integer
ex2 = IdentityT (Just 1) >>= (return . (+1))


