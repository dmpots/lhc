{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}
module Grin.Types
    ( module Grin.Types
    , module Grin.SimpleCore.Types
    ) where

import CompactString

import Grin.SimpleCore.Types (Lit(..))

import Data.Binary
import Data.DeriveTH
import Control.Monad  (ap)

import Traverse

-- Invariants:
--   The nodes referred to by the functions are a subset of the nodes in 'grinNodes'.
data Grin
    = Grin { grinNodes      :: [NodeDef]
           , grinCAFs       :: [CAF]
           , grinFunctions  :: [FuncDef]
           , grinEntryPoint ::Renamed
           , grinUnique     :: Int
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
    = ConstructorNode
    | FunctionNode
    deriving (Show,Eq,Ord)

data Type
    = PtrType
    | WordType
    | NodeType
    deriving (Eq)

data Lambda = Renamed :-> Expression deriving Show
data Alt = Value :> Expression deriving Show

infixr 1 :->
infixr 1 :>>=
infixr 1 :>>

data Expression
    = Expression :>>= Lambda
    | Expression :>> Expression
    | Application { expFunction :: Renamed
                  , expArgs     :: [Renamed] }
    | Case        { expValue    :: Renamed
                  , expAlts     :: [Alt] }
    | Store       Value
    | Unit        Value
    deriving Show

instance Traverse Expression where
    tmapM fn exp
        = case exp of
            e1 :>>= bind :-> e2
              -> do e1' <- fn e1
                    e2' <- fn e2
                    return (e1' :>>= bind :-> e2')
            e1 :>> e2
              -> do e1' <- fn e1
                    e2' <- fn e2
                    return (e1' :>> e2')
            Application{}
              -> return exp
            Case scrut alts
              -> do alts' <- sequence [ do alt' <- fn alt
                                           return (cond :> alt')
                                        | cond :> alt <- alts]
                    return $ Case scrut alts'
            Store{}
              -> return exp
            Unit{}
              -> return exp


type Variable = CompactString

-- FIXME: Writer manual Eq and Ord instances for Renamed.
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

numbered :: Renamed -> Bool
numbered Aliased{} = True
numbered Anonymous{} = True
numbered _ = False

uniqueId :: Renamed -> Int
uniqueId (Aliased uid _name) = uid
uniqueId (Anonymous uid)     = uid
uniqueId (Builtin prim)      = error $ "Grin.Types.uniqueId: Primitive: " ++ show prim
uniqueId (External fn)       = error $ "Grin.Types.uniqueId: External: " ++ show fn

data Value
    = Node Renamed NodeType Int [Renamed]
    | Vector [Renamed]
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
$(derive makeBinary ''Alt)
$(derive makeBinary ''Lambda)
$(derive makeBinary ''Grin)

