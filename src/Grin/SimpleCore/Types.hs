{-# LANGUAGE TemplateHaskell #-}
module Grin.SimpleCore.Types where

import CompactString
import Traverse
import Language.Core (Tdef(..),Cdef(..),Kind(..),Ty(..))
import qualified Language.Core as Core
import Control.Monad

import Data.Binary
import Data.DeriveTH

data SimpleModule
    = SimpleModule { modulePackage :: String
                   , moduleName    :: String
                   , moduleTypes   :: [Tdef]
                   , moduleDefs    :: [SimpleDef]
                   , moduleDeps    :: [(String,String)] -- List of (pkg,module)
                   } deriving (Show,Read)

data SimpleDef
    = SimpleDef { simpleDefName :: CompactString
                , simpleDefArgs :: [CompactString]
                , simpleDefBody :: SimpleExp }
    deriving (Show,Read)
simpleDefArity :: SimpleDef -> Int
simpleDefArity = length . simpleDefArgs

data SimpleExp
    = Var CompactString
    | Primitive CompactString
    | Dcon CompactString
    | Lit Lit
    | App SimpleExp SimpleExp
    | Let CompactString CompactString [CompactString] Int SimpleExp
    | LetRec [(CompactString, CompactString, [CompactString], Int)] SimpleExp
    | LetStrict CompactString SimpleExp SimpleExp
    | Case SimpleExp CompactString [Alt] (Maybe SimpleExp)
    | External String String
    | DynExternal String
    | Label String
    | Note String SimpleExp
      deriving (Show,Read)

data Alt
    = Acon CompactString [CompactString] SimpleExp
    | Alit Lit SimpleExp
    | Adefault SimpleExp
    deriving (Show,Read)

data Lit
    = Lint Integer
    | Lrational Rational
    | Lchar Char
    | Lstring String
    deriving (Show,Read,Eq,Ord)

$(derive makeBinary ''Alt)
$(derive makeBinary ''Lit)
$(derive makeBinary ''SimpleExp)
$(derive makeBinary ''SimpleDef)
$(derive makeBinary ''Tdef)
$(derive makeBinary ''Ty)
$(derive makeBinary ''Kind)
$(derive makeBinary ''Cdef)
$(derive makeBinary ''SimpleModule)


instance Traverse Core.Exp where
    tmapM fn exp = case exp of
        Core.Var{}  -> return exp
        Core.Dcon{} -> return exp
        Core.Lit{}  -> return exp
        Core.App a b -> return Core.App `ap` fn a `ap` fn b
        Core.Appt a t -> return Core.Appt `ap` fn a `ap` return t
        Core.Lam b e -> return (Core.Lam b) `ap` fn e
        Core.Lamt b e -> return (Core.Lamt b) `ap` fn e
        Core.Let vdefg e -> return Core.Let `ap` return vdefg `ap` fn e
        Core.Case e bind ty alts
          -> let mapAlt (Core.Acon qual tbinds vbinds e)
                     = return (Core.Acon qual tbinds vbinds) `ap` fn e
                 mapAlt (Core.Alit lit e)
                     = return (Core.Alit lit) `ap` fn e
                 mapAlt (Core.Adefault e)
                     = return Core.Adefault `ap` fn e
             in return Core.Case `ap` fn e `ap` return bind `ap` return ty `ap` mapM mapAlt alts
        Core.Cast e ty -> return Core.Cast `ap` fn e `ap` return ty
        Core.Note n e  -> return (Core.Note n) `ap` fn e
        Core.External{} -> return exp
        Core.DynExternal{} -> return exp
        Core.Label{} -> return exp

