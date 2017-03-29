module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Identity

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
--embedded' = MaybeT $ ExceptT $ ReaderT $ (const (Right (Just 1)))
embedded' = (MaybeT . ExceptT . ReaderT) $ return . (const (Right (Just 1)))

bar :: () -> Either String (Maybe Int)
bar = const (Right (Just 1))

mt :: MaybeT (Either String) Int
mt = MaybeT (Right (Just 1))

et :: ExceptT String Identity (Maybe Int)
et = ExceptT (Identity (Right (Just 1)))

met :: MaybeT (ExceptT String Identity) Int
met = MaybeT et

f :: () -> Either String (Maybe Int)
f = const (Right (Just 1))

rt = ReaderT f


x1 :: MaybeT (ExceptT String Identity) Int
x1 = (MaybeT . ExceptT) (Identity (Right (Just 1)))


f1 :: (r -> m (Either e (Maybe a))) -> MaybeT (ExceptT e (ReaderT r m)) a
f1 = MaybeT . ExceptT . ReaderT

f2 :: (() -> IO (Either String (Maybe Int))) -> MaybeT (ExceptT String (ReaderT () IO)) Int
f2 = f1


x2 :: MaybeT (ExceptT String (ReaderT () Identity)) Int
x2 = (MaybeT . ExceptT . ReaderT) (Identity <$> bar)


f3 :: (() -> IO (Either String (Maybe Int)))
f3 = return . bar

x3 = (MaybeT . ExceptT . ReaderT) f3
