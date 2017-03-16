module Cipher where

import Data.Char

caeser :: Int -> String -> String
caeser _ [] = []
--caeser s (x:xs) = chr $ addMod26 s $ ord x : caeser s xs

addMod26 :: Int -> Int -> Int
addMod26 x y = (x + y) `mod` 26

