module GHC.Integer.Internals where

import GHC.Prim
import GHC.Types

import GHC.Integer.PureInteger

newtype Integer = Integer LargeBaseLEList

