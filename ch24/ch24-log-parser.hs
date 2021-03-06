{-# LANGUAGE QuasiQuotes #-}

import Text.Printf
import Text.RawString.QQ
import Text.Trifecta
import Control.Applicative
import Data.List

type Year = Integer
type Month = Integer
type Day = Integer

newtype Date = Date (Year,Month,Day)
newtype Time = Time Integer

data Activity = Activity Time String

data OneDay = OneDay Date [Activity] 
newtype Log = Log [OneDay]

parseComment :: Parser ()
parseComment = char '-' >> char '-' >> skipMany (notChar '\n') <* char '\n'

parseDate :: Parser Date
parseDate = do
    _ <- string "# "
    year <- natural
    _ <- char '-'
    mon <- natural
    _ <- char '-'
    day <- natural
    return $ Date (year,mon,day)

parseTime :: Parser Time
parseTime = do
    h <- count 2 digit 
    _ <- char ':'
    m <- count 2 digit
    let h' = read h
        m' = read m
    if h' > 23 then fail "hour out of range" else
        if m' > 59 then fail "minute out of range" else
            return $ Time $ (h'*60) + m'

stop :: Parser a
stop = unexpected "stop"

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
    skipMany (do _ <- try (char '-'  >> char '-')
                 skipMany (noneOf "\n")
                 skipEOL)

parseActivity' :: Parser Activity
parseActivity' = do
    t <- parseTime
    _ <- char ' '
    act <- some (noneOf "\n") 
    return $ Activity t act

parseActivity :: Parser Activity
parseActivity = do
    t <- parseTime
    _ <- char ' '
    act <- some (noneOf "\n") <?> "act"
    skipEOL
    -- notFollowedBy (skipComments <|> skipEOL) <?> "foo"
    return $ Activity t act

skipWhitespace :: Parser ()
--skipWhitespace = skipMany (char ' ' <|> char '\n') 
skipWhitespace = skipMany (oneOf " \n")

parseLines :: Parser [String]
parseLines = many $ many (noneOf "\n") <* char '\n'

parseOneDay :: Parser OneDay
parseOneDay = do
    skipWhitespace <?> "whitespace"
    skipComments <?> "comments"
    d <- parseDate <* skipComments  <?> "date"
    as <- many parseActivity  <?> "activities"
    return $ OneDay d as

parseLog :: Parser Log
parseLog = Log <$> many parseOneDay

instance Show Date where
    show (Date (y,m,d)) = 
        printf "# %04d-%02d-%02d\n" y m d

instance Show OneDay where
    show (OneDay date activities) = 
        show date ++ "\n" ++ concatMap show activities

instance Show Activity where
    show (Activity ts note) =
        show ts ++ " " ++ note ++ "\n"

instance Show Time where
    show (Time minute) = printf "%02d:%02d" h m
        where (h,m) = divMod minute 60

instance Show Log where
    show (Log xs) = concatMap ((++ "\n") . show) xs

sampleLog :: String
sampleLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
a|]

sampleData :: Result Log
sampleData = parseString parseLog mempty sampleLog

-- won't work because parser discards comments
parseLogTest s = case parseString parseLog mempty s of
    Failure _ -> False
    Success l -> show l == s

com :: Parser Char
com = char '-' >> char '-' >> many (notChar '\n') >> char '\n'
com1 = try com <|> anyChar
com2 = many com1

comment :: Parser Char
comment = do
    char '-'
    char '-'
    many (notChar '\n')
    char '\n'

item :: Parser Char
item = try comment <|> anyChar

mySatisfy :: (Char -> Bool) -> Parser Char
mySatisfy f = do
    c <- item
    if f c then return c else fail "mySatisfy"

myChar :: Char -> Parser Char
myChar c = mySatisfy (== c)
