module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List

import UnitTests

main = defaultMain tests

tests = [ testGroup "Unit tests"
           unitTests
        ]
