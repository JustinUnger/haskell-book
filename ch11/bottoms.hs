
-- don't do this: 

{-
data Automobile = Null
                | Car { make :: String
                      , model :: String
                      , year :: Integer }
                deriving (Eq, Show)
-}
-- this is a runtime exception *boom*
-- > make $ Null
-- "*** Exception: No match in record selector make

-- do this instead: 

data Car = Car { make :: String
               , model :: String
               , year :: Integer } 
               deriving (Eq, Show)

data Automobile = Null
                | Automobile Car
                deriving (Eq, Show)

{-

now we get a type check error on compile, rather than a runtime exception

*Main> make Null

<interactive>:113:6: error:
    • Couldn't match expected type ‘Car’ with actual type ‘Automobile’
    • In the first argument of ‘make’, namely ‘Null’
      In the expression: make Null
      In an equation for ‘it’: it = make Null
-}

