{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}
module Grin.SimpleCore.Types where

import CompactString
import Traverse
import qualified Language.Core as Core
import Control.Monad

import Data.Binary
import Data.DeriveTH

data SimpleModule
    = SimpleModule { modulePackage :: String
                   , moduleName    :: String
                   , moduleTypes   :: [SimpleType]
                   , moduleEnums   :: [SimpleEnum]
                   , moduleDefs    :: [SimpleDef]
                   }

type ModuleIdent = (String,String)
moduleIdent mod = (modulePackage mod, moduleName mod)

data SimpleType
    = SimpleType { simpleTypeName  :: CompactString
                 , simpleTypeArity :: Int
                 }

data SimpleEnum
    = SimpleEnum { simpleEnumName :: CompactString
                 , simpleEnumMembers :: [CompactString]
                 }

data SimpleDef
    = SimpleDef { simpleDefName :: CompactString
                , simpleDefArgs :: [CompactString]
                , simpleDefBody :: SimpleExp
                , simpleDefDeps :: [(String,String)]
                }
simpleDefArity :: SimpleDef -> Int
simpleDefArity = length . simpleDefArgs

data SimpleExp
    = Var CompactString Bool
    | Primitive CompactString
    | EnumPrimitive CompactString CompactString Ty
    | Dcon CompactString
    | Lit Lit
    | App SimpleExp [SimpleExp]
    | Let CompactString CompactString [CompactString] Int SimpleExp
    | LetRec [(CompactString, CompactString, [CompactString], Int)] SimpleExp
    | LetStrict CompactString SimpleExp SimpleExp
    | Case SimpleExp CompactString [Alt]
    | CaseStrict SimpleExp CompactString [Alt]
    | External String String [FFIType]
    | DynExternal String [FFIType]
    | Label String
    | Note String SimpleExp

data Ty = Tcon CompactString

data FFIType = Word | Int | Addr | Unit | Invalid

data Alt
    = Acon CompactString [CompactString] SimpleExp
    | Alit Lit SimpleExp
    | Adefault SimpleExp

data Lit
    = Lint Integer
    | Lrational Rational
    | Lchar Char
    | Lstring String
    deriving (Show,Eq,Ord)

$(derive makeBinary ''Alt)
$(derive makeBinary ''Lit)
$(derive makeBinary ''Ty)
$(derive makeBinary ''FFIType)
$(derive makeBinary ''SimpleExp)
$(derive makeBinary ''SimpleDef)
$(derive makeBinary ''SimpleType)
$(derive makeBinary ''SimpleEnum)
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

