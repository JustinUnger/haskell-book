{-# LANGUAGE QuasiQuotes #-}

-- this version can deal with line comments and uses nanoparsec

import Text.Printf
import Text.RawString.QQ
import NanoParsec
import Control.Applicative
import Data.List

type Year = Int
type Month = Int
type Day = Int

newtype Date = Date (Year,Month,Day)
newtype Time = Time Int

data Activity = Activity Time String

data OneDay = OneDay Date [Activity] 
newtype Log = Log [OneDay]

--skipMany :: Alternative f => f a -> f ()
--skipMany pa = () <$ many pa
skipMany pa = many pa >> return ()

count :: Applicative m => Int -> m a -> m [a]
count 0 m = pure []
count n m = (:) <$> m <*> count (n-1) m

oneOf :: [Char] -> Parser Char
oneOf cs = mySatisfy (flip elem cs)

noneOf :: [Char] -> Parser Char
noneOf cs = mySatisfy (not . flip elem cs)

notChar :: Char -> Parser Char
notChar c = mySatisfy (/= c)

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
    h <- read <$> count 2 digit 
    _ <- char ':'
    m <- read <$> count 2 digit
    if h > 23 then fail "hour out of range" else
        if m > 59 then fail "minute out of range" else
            return $ Time $ (h*60) + m

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseActivity :: Parser Activity
parseActivity = do
    t <- parseTime
    _ <- char ' '
    act <- some (noneOf "\n")
    skipEOL
    return $ Activity t act

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \n")

parseOneDay :: Parser OneDay
parseOneDay = do
    skipWhitespace
    d <- parseDate
    skipWhitespace
    as <- many parseActivity
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

comment :: Parser Char
comment = do
    char '-'
    char '-'
    many (notChar '\n')
    char '\n'

myItem :: Parser Char
myItem = comment <|> item

mySatisfy :: (Char -> Bool) -> Parser Char
mySatisfy f = do
    c <- myItem
    if f c then return c else failure

myChar :: Char -> Parser Char
myChar c = mySatisfy (== c)

sampleData :: Log
sampleData = runParser parseLog sampleLog

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
|]
