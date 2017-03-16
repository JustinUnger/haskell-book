> {-# LANGUAGE InstanceSigs #-}

> newtype Reader r a = Reader { runReader :: (r -> a) }

> instance Functor (Reader r) where
>   fmap f (Reader ra) = Reader $ \r -> f (ra r)

> instance Applicative (Reader r) where
>   pure :: a -> Reader r a
>   pure a = Reader $ (\_ -> a)

>   (<*>) :: Reader r (a -> b) 
>         -> Reader r a
>         -> Reader r b

>   (Reader rab) <*> (Reader ra) = 
>       Reader $ \r -> rab r (ra r)

implement the reader monad:

> instance Monad (Reader r) where
>   return = pure

>   (>>=) :: Reader r a
>         -> (a -> Reader r b)
>         -> Reader r b
>   (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

(r -> a) -> (a -> r -> b) -> (r -> b)

join m = m >>= id

   (Reader ra) >>= aRb = (Reader $ \r  -> aRb (ra r)) >>= id


> newtype HumanName = HumanName String deriving (Eq, Show)
> newtype DogName = DogName String deriving (Eq, Show)
> newtype Address = Address String deriving (Eq, Show)

> data Person = Person {
>	  humanName :: HumanName
>	, dogName :: DogName
>	, address :: Address
>	} deriving (Eq, Show)

> data Dog = Dog {
>	  dogsName :: DogName
>	, dogsAddress :: Address
>	} deriving (Eq, Show)

> pers :: Person
> pers =
>	 Person (HumanName "Big Bird")
>		(DogName "Barkley")
>		(Address "Sesame Street")	

> chris :: Person
> chris = Person (HumanName "Chris Allen")
>		 (DogName "Papu")
>		 (Address "Austin")

 getDogRM :: Person -> Dog

getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog (name) (addy)

> getDogRM' :: Reader Person Dog
> getDogRM' = 
>    Reader dogName >>= (\dn -> Reader address >>= (\addy -> return (Dog dn addy)))


> getDogRM'' :: Person -> Dog
> getDogRM'' = runReader $ do
>   name <- Reader dogName
>   addy <- Reader address
>   return (Dog name addy)

> getDogRM''' :: Reader Person Dog
> getDogRM''' = do
>   name <- Reader dogName
>   addy <- Reader address
>   return (Dog name addy)


