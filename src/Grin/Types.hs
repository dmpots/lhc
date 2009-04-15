{-# LANGUAGE TemplateHaskell #-}
module Grin.Types
    ( module Grin.Types
    , module Grin.SimpleCore.Types
    ) where

import CompactString

import Grin.SimpleCore.Types (Lit(..))

import Data.Binary
import Data.DeriveTH
import Control.Monad  (ap)


-- Invariants:
--   The nodes referred to by the functions are a subset of the nodes in 'grinNodes'.
data Grin
    = Grin { grinNodes     :: [NodeDef]
           , grinCAFs      :: [CAF]
           , grinFunctions :: [FuncDef]
           }

data CAF
    = CAF { cafName  :: Renamed
          , cafValue :: Value
          }

data FuncDef
    = FuncDef { funcDefName :: Renamed
              , funcDefArgs :: [Renamed]
              , funcDefBody :: Expression
              }

data NodeDef
    = NodeDef { nodeName :: Renamed
              , nodeType :: NodeType
              , nodeArgs :: [Type]
              }

{-
  ConstructorNodes represent data, like: Nil, Cons, Char, etc.
  FunctionNodes represents suspended functions which may be partially applied.
-}
data NodeType
    = ConstructorNode Int
    | FunctionNode Int    -- Function node with number of 'missing' arguments.
    deriving (Show,Eq,Ord)

data Type
    = PtrType
    | WordType
    | NodeType
    deriving (Eq)

data Lambda = Value :-> Expression

infixr 1 :->
infixr 1 :>>=

data Expression
    = Expression :>>= Lambda
    | Application { expFunction :: Renamed
                  , expArgs     :: [Value] }
    | Case        { expValue    :: Value
                  , expAlts     :: [Lambda] }
    | Store       Value
    | Unit        Value

type Variable = CompactString

data Renamed = Aliased Int CompactString
             | Anonymous Int
             | Builtin CompactString
             | External String
    deriving (Show,Eq,Ord)

isAliased, isBuiltin, isExternal :: Renamed -> Bool

isAliased Aliased{} = True
isAliased _ = False

isBuiltin Builtin{} = True
isBuiltin _ = False

isExternal External{} = True
isExternal _ = False

alias :: Renamed -> Maybe CompactString
alias (Aliased _ name) = Just name
alias _ = Nothing

data Value
    = Node Renamed NodeType [Value]
    | Vector [Value]
    | Lit Lit
    | Variable Renamed
    | Hole Int
    | Empty
    deriving (Show,Eq)


$(derive makeBinary ''NodeType)
$(derive makeBinary ''Renamed)
$(derive makeBinary ''Value)
$(derive makeBinary ''CAF)
$(derive makeBinary ''FuncDef)
$(derive makeBinary ''NodeDef)
$(derive makeBinary ''Expression)
$(derive makeBinary ''Type)
$(derive makeBinary ''Lambda)
$(derive makeBinary ''Grin)

