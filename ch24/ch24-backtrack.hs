-- use OverloadedStrings so that string literals are of type ByteString

{-# LANGUAGE OverloadedStrings #-}

module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

-- first parser, attempts to parse '1' followed by '2' /or/ '3'. 
-- does not backtrack

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

-- this one will backtrack if the first parse fails 

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12")
    <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
    trifP nobackParse "13"
    trifP tryParse "13"
    
    parsecP nobackParse "13"
    parsecP tryParse "13"
    
    attoP nobackParse "13"
    attoP tryParse "13"



