> newtype Name = Name String deriving (Eq, Show)
> newtype Address = Address String deriving (Eq, Show)

> validateLength :: Int -> String -> Maybe String
> validateLength maxLen s =
>   if (length s ) > maxLen
>   then Nothing
>   else Just s

> mkName :: String -> Maybe Name
> mkName s = fmap Name $ validateLength 25 s

> mkAddress :: String -> Maybe Address
> mkAddress a = fmap Address $ validateLength 100 a

> data Person =
>   Person Name Address
>   deriving (Eq, Show)

annoying naive version of mkPerson without applicative: 

> mkPerson :: String -> String -> Maybe Person
> mkPerson n a =
>   case mkName n of
>       Nothing -> Nothing
>       Just n' -> 
>           case mkAddress a of
>               Nothing -> Nothing
>               Just a' -> Just $ Person n' a'


use like this: 

fmap Person (mkName "Joe") <*> mkAddress "123 Fake St."

-or-

> mp :: Maybe Person
> mp = Person <$> mkName "Joe" <*> mkAddress "123 Fake St."


simply mkPerson like this: 

> mkPerson' :: String -> String -> Maybe Person
> mkPerson' n a = Person <$> mkName n <*> mkAddress a

