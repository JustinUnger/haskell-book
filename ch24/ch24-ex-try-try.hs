{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

-- test inputs

badFraction :: String
badFraction = "1/0"

alsoBad :: String
alsoBad = "10"

shouldWork :: String
shouldWork = "1/2"

shouldAlsoWork :: String
shouldAlsoWork = "2/1"

-- actual parser

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator can't be zero"
        _ -> return (numerator % denominator)

-- exercise: try try

type RationalOrDouble = Either Rational Double

parseNum :: Parser RationalOrDouble
parseNum = (try $ Left <$> virtuousFraction) <|> (Right <$> double)

parseNum' :: Real a => Parser a
--parseNum' = try (fromRational <$> virtuousFraction) <|> double
parseNum' = undefined

parseRational :: Parser Rational
parseRational = try virtuousFraction <|> (toRational <$> decimal)

main :: IO ()
main = do
    print $ parseString parseNum mempty "123.45"
    print $ parseString parseNum mempty "12345/100"

