module GHC.Integer.Internals where

import GHC.Prim
import GHC.Types

import GHC.Integer.PureInteger

data Integer = Integer LargeBaseLEList

