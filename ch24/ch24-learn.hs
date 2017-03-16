import Text.Trifecta
import Text.Parser.Combinators ()
import Control.Applicative

stop :: Parser a
stop = unexpected "myStop"

one :: Parser Char
one = char '1'

one' :: Parser b
one' = one >> stop

two :: Parser Char
two = char '2'

two' :: Parser b
two' = two >> stop

three :: Parser Char
three = char '3'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

oneTwoThree :: Parser Char
oneTwoThree = char '1' >> char '2' >> char '3'

testParse :: Parser Char -> IO ()
testParse p = 
    print $ parseString p mempty "123"

-- exercise 1: make parsers one and oneTwo fail because they didn't exhaust
-- the input stream

oneEof :: Parser ()
oneEof = one >> eof

oneTwoEof :: Parser ()
oneTwoEof = oneTwo >> eof

oneTwoThreeEof :: Parser ()
oneTwoThreeEof = oneTwoThree >> eof

strOne :: Parser String
strOne = string "1"

strTwo :: Parser String
strTwo = string "2"

strOneTwo :: Parser String
strOneTwo = strOne >> strTwo

-- exercise 2
-- use string to make a parser. a single parser should work with 
-- all three strings "1","12","123"

ex2 :: Parser String
ex2 = string "123" <|> string "12" <|> string "1" <|> stop

foo :: Parser String
foo = string "123" <|> string "12" >> stop  <|> string "1"

-- exercise 3

-- write a parser that does what string does, but using char

ex3 :: String -> Parser String
ex3 s = sequenceA $ fmap char s

string' :: String -> Parser String
string' = sequenceA . fmap char

ps :: [Parser Char]
ps = [char '1', char '2', char '3']

str123 :: Parser [Char]
str123 = sequenceA ps

myStr123 :: Parser String
myStr123 = ex3 "123"

main' :: IO ()
main' = do
    print $ parseString str123 mempty "123"  
    print $ parseString myStr123 mempty "123" 
