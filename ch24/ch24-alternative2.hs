{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = 
    Either Integer String

-- here document: haskell calls this QuasiQuoter

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

-- this one will eat any preceeding newlines
-- and it will barf if it doesn't get an integer or letter as the first thing
-- after eating newlines

parseNos' :: Parser NumberOrString
parseNos' = 
    skipMany (oneOf "\n")
    >>       (Left <$> integer) <|> (Right <$> some letter)


-- this one will eat newlines at the end of the file as well

parseNos'' :: Parser NumberOrString
parseNos'' = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

main = do
    -- parseNos can't deal with preceeding newlines
    print $ parseString parseNos mempty eitherOr
    -- parseNos' can, but only returns one value
    print $ parseString parseNos' mempty eitherOr
    -- some parseNos' can't deal with trailing newlines
    print $ parseString (some parseNos') mempty eitherOr
    -- (some parseNos'') can
    print $ parseString (some parseNos'') mempty eitherOr

 -- another way to make the original parseNos' deal with trailing newlines:

    print $ parseString (some (token parseNos')) mempty eitherOr

