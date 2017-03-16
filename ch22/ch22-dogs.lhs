> import Control.Applicative (liftA2)

> newtype HumanName = HumanName String deriving (Eq, Show)

> newtype DogName = DogName String deriving (Eq, Show)

> newtype Address = Address String deriving (Eq, Show)

you could do this with String -> String -> String, but it sucks 
if each string doesn't represent the same thing or is treated differently
make it explicit with newtype wrappers

> data Person = Person {
>	  humanName :: HumanName
>	, dogName :: DogName
>	, address :: Address
>	} deriving (Eq, Show)

> data Dog = Dog {
>	  dogsName :: DogName
>	, dogsAddress :: Address
>	} deriving (Eq, Show)

some sample data: 

> pers :: Person
> pers =
>	 Person (HumanName "Big Bird")
>		(DogName "Barkley")
>		(Address "Sesame Street")	

> chris :: Person
> chris = Person (HumanName "Chris Allen")
>		 (DogName "Papu")
>		 (Address "Austin")

without Reader:

> getDog :: Person -> Dog
> getDog p = Dog (dogName p) (address p)

with reader: 

> getDogR :: Person -> Dog
> getDogR = Dog <$> dogName <*> address

this looks like: 

fmap Dog dogName
 -or-

Dog . dogName == (\x -> Dog (dogName x))

(\x -> Dog (dogName x)) <*> address
       rab              <*> ra

rab ~ r    -> (   a    ->  b ) 
    Person -> (Address -> Dog)

ra ~ r    ->    a 
   Person -> Address

rb ~  r    ->  b
    Person -> Dog
     

Reader $ \r -> (\x -> Dog (dogName x)) r (address r)
Reader $ \r -> rab r (ra r)

we can write fmap in terms of applicative: 

> fmap' :: Applicative f => (a -> b) -> f a -> f b
> fmap' f x = pure f <*> x

pure f = (\_ -> f)

pure (+1) <*> (*2)
\r -> (\_ -> (+1)) r ((*2) r) 

this way of using Applicative for Reader is common, this is an alternate: 

> getDogR' :: Person -> Dog
> getDogR' = liftA2 Dog dogName address

this lets us avoid threading Person parameter "p" thru our expressions. 
"let the types manage it for us"

compare

getDog p = Dog (dogName p) (address p)
gotDogR = Dog <$> dogName <*> address

-- with Reader Monad

> getDogRM :: Person -> Dog
> getDogRM = do
>    name <- dogName
>    addy <- address
>    return $ Dog name addy

> getDogRM' = dogName >>= \name -> address >>= \addy -> return $ Dog name addy

