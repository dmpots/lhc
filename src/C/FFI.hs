module C.FFI(
    CallConv(..),
    Safety(..),
    FfiType(..),
    FfiExport(..),
    FfiSpec(..),
    Requires(..)
    ) where
import Data.Typeable
import Data.DeriveTH
import Data.Derive.All
import Data.Binary
import Data.Monoid
import Control.Monad

type CName    = String

data CallConv = CCall | StdCall | Primitive | DotNet deriving(Eq,Ord,Show)
$(derive makeBinary ''CallConv)

data Safety = Safe | Unsafe deriving(Eq,Ord,Show)
$(derive makeBinary ''Safety)


data Requires = Requires {
    reqIncludes :: [String],
    reqLibraries :: [String]
    } deriving(Eq, Ord)

instance Show Requires where
    show (Requires [] []) = "()"
    show (Requires xs ys) = show (xs,ys)

-- we have to put these after so the instances go in order
$(derive makeBinary ''Requires)
$(derive makeMonoid ''Requires)

data FfiType = Import CName Requires
             | ImportAddr CName Requires
             | Wrapper
             | Dynamic
             deriving(Eq,Ord,Show)
$(derive makeBinary ''FfiType)


data FfiSpec = FfiSpec FfiType Safety CallConv
             deriving(Eq,Ord,Show)
$(derive makeBinary ''FfiSpec)

data FfiExport = FfiExport CName Safety CallConv
             deriving(Eq,Ord,Show,Typeable)
$(derive makeBinary ''FfiExport)

