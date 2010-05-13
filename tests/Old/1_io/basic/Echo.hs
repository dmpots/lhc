module Main where

import Data.Char

main :: IO ()
main = interact (map toUpper)
