-- RUN: %lhc -c %s 
-- RUN: %lhc compile %b.hcr
-- RUN: %b > %t1 ; diff %b.expected.stdout %t1
-- XFAIL: *

module Main where

import Data.Char

main :: IO ()
main = interact (map toUpper)
