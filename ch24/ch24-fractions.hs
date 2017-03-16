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


-- exercise: unit of success

myParser :: Parser Integer
myParser = integer <* eof 

main :: IO ()
main = do
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad
    print $ parseString parseFraction mempty badFraction
    print $ parseString myParser mempty "123"

