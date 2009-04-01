module Grin.Types where

import CompactString

-- Invariants:
--   The nodes referred to by the functions are a subset of the nodes in 'grinNodes'.
data Grin
    = Grin { grinNodes :: [NodeDef]
           , grinFunctions :: [FuncDef]
           , grinUnique :: Int
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
    deriving (Show,Read)

data Type
    = PtrType
    | WordType
    | NodeType

data Lambda = Value :-> Expression

infixr 1 :->
infixr 1 :>>=

data Expression
    = Expression :>>= Lambda
    | Application { expFunction :: Renamed
                  , expArgs     :: [Value] }
    | Case        { expValue    :: Value
                  , expAlts     :: [Lambda] }
    | Fetch       Variable
    | Store       Value
    | Unit        Value

type Variable = CompactString

data Renamed = Aliased Int CompactString
             | Anonymous Int
             | Builtin CompactString
    deriving (Show,Read,Eq,Ord)

isBuiltin Builtin{} = True
isBuiltin _ = False

data Value
    = Node Renamed NodeType [Value]
    | Integer Integer
    | Rational Rational
    | Char Char
    | String String
    | Variable Renamed
    | Hole Int
    | Empty
    deriving (Show,Read)



