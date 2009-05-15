module Foreign.C.Types where

import GHC.Base
import GHC.Num
import GHC.Int
import GHC.Word
import GHC.Show
import GHC.Real
import GHC.Enum
import Data.Bits
import {-# SOURCE #-} Foreign.Storable

newtype CInt = CInt Int32 deriving (Show,Eq,Ord,Num,Storable,Integral,Real,Enum,Bits)
newtype CSize = CSize Word64 deriving (Show,Eq,Ord,Num,Storable,Integral,Real,Enum,Bits)
newtype CChar = CChar Int8 deriving (Show,Eq,Ord,Num,Storable,Integral,Real,Enum,Bits)
newtype CWchar = CWchar Int32 deriving (Show,Eq,Ord,Num,Storable,Integral,Real,Enum,Bits)
newtype CClock = CClock Int64 deriving (Show,Eq,Ord,Num,Storable,Integral,Real,Enum,Bits)
newtype CTime = CTime Int64 deriving (Show,Eq,Ord,Num,Storable,Integral,Real,Enum,Bits)
newtype CLong = CLong Int64 deriving (Show,Eq,Ord,Num,Storable,Integral,Real,Enum,Bits)
