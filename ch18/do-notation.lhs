import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *> putStrLn "another thing"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' = do
    getLine >>= putStrLn


bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("hello there: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>= 
    \name -> putStrLn ("hello there: " ++ name)


twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:
    age <- getLine
    putStrLn ("hello there: " ++ name ++ " who is: " ++ age ++ "years old.")
    
twoBinds' :: IO ()
twoBinds' =
    putStrLn "name pls:" >>
    getLine >>=
    \name -> putStrLn "age pls:" >>
    getLine >>=
    \age ->
    putStrLn ("hello there: " ++ name ++ " who is: " ++ age ++ " years old.")


> twiceWhenEven :: [Integer] -> [Integer]
> twiceWhenEven xs = do
>   x <- xs
>   if even x
>       then [x*x, x*x]
>--     else [x*x]
>       else []

> twiceWhenEven' :: [Integer] -> [Integer]
> twiceWhenEven' xs =
>   xs >>= 
>       \x -> if even x
>                then [x*x, x*x]
>                else [x*x]

