{-# LANGUAGE OverloadedStrings #-}
module Grin.Optimize.Inline
    ( inlinePass
    ) where

import Grin.Types
import Traverse
import Grin.Transform

import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Debug.Trace
import Text.Printf
import Control.Applicative

import qualified Data.Map as Map

inlinePass :: Grin -> Grin
inlinePass = inlineSimple . inlineCAFs


-- Lower cheap CAFs to regular functions.
inlineCAFs :: Grin -> Grin
inlineCAFs grin
    = let toInline = map funcDefName $ filter (\def -> funcCategory def `elem` [Cheap]) (grinFunctions grin)
          cafsToInline = Map.fromList [ (cafName caf, tag) | caf@CAF{cafValue=Node tag FunctionNode 0 []} <- grinCAFs grin, tag `elem` toInline ]
      in runTrans (runReaderT (inlineCAFs' grin) cafsToInline) grin

type M = ReaderT (Map.Map Renamed Renamed) Transform

inlineCAFs' :: Grin -> M Grin
inlineCAFs' = transformExp inlineCAF

inlineCAF :: Expression -> M Expression
inlineCAF (Application fn args)
    = inlineArgs args (Application fn)
inlineCAF (Unit v)
    = inlineValue Unit v
inlineCAF (Store v)
    = inlineValue Store v
inlineCAF e = tmapM inlineCAF e

inlineValue fn (Variable v)
    = inlineArgs [v] $ \[v'] -> fn (Variable v')
inlineValue fn (Node tag nt missing args)
    = inlineArgs args $ \args' -> fn (Node tag nt missing args')
inlineValue fn (Vector args)
    = inlineArgs args $ \args' -> fn (Vector args')
inlineValue fn value
    = return $ fn value

inlineArgs args fn
    = do m <- ask
         let worker acc []     = return (fn (reverse acc))
             worker acc (x:xs) = case Map.lookup x m of
                                   Nothing  -> worker (x:acc) xs
                                   Just caf -> do v <- newVariable
                                                  rest <- worker (v:acc) xs
                                                  return $ Store (Node caf FunctionNode 0 []) :>>= v :-> rest 
         worker [] args






---------------------------------
-- Inline cheap functions.


inlineSimple :: Grin -> Grin
inlineSimple grin
    = let inp = Map.fromList [ (funcDefName def, (funcCategory def, def)) | def <- grinFunctions grin ]
      in runTrans (runReaderT (transformExp inlineSimpleExp grin) inp) grin

type Simple = ReaderT (Map.Map Renamed (Category, FuncDef)) Transform

inlineSimpleExp :: Expression -> Simple Expression
inlineSimpleExp e@(Store (Node tag FunctionNode 0 args))
    = do mbEntry <- findFunc tag
         case mbEntry of
           Just (Cheap, func) -> lazify <$> doInline func args
           _ -> return e
inlineSimpleExp e = tmapM inlineSimpleExp e

doInline func args
    = do let renamedArgs = Map.fromList (zip (funcDefArgs func) (args ++ repeat (Builtin "undefined")))
         lift (renameExp renamedArgs (funcDefBody func))

lazify (e1 :>>= bind :-> e2)
    = e1 :>>= bind :-> lazify e2
lazify (e1 :>> e2)
    = e1 :>> lazify e2
lazify (Application fn args) | not (isBuiltin fn) && not (isExternal fn)
    = Store (Node fn FunctionNode 0 args)
lazify (Unit v)
    = Store v
lazify (Application (Builtin "eval") [arg])
    = Unit (Variable arg)
lazify e
    = e

findFunc :: Renamed -> Simple (Maybe (Category, FuncDef))
findFunc name
    = asks $ Map.lookup name







---------------------------------
-- Other stuff


threshold = 10

funcSize :: FuncDef -> Int
funcSize def = expressionSize (funcDefBody def)

expressionSize :: Expression -> Int
expressionSize (e1 :>>= bind :-> e2)
    = expressionSize e1 + expressionSize e2
expressionSize (e1 :>> e2)
    = expressionSize e1 + expressionSize e2
expressionSize (Application fn args)
    = 1
expressionSize (Case scrut alts)
    = sum [ expressionSize branch | _ :> branch <- alts ]
expressionSize Store{}
    = 1
expressionSize Unit{}
    = 1

data Category = NoInline | Lazy | Strict | Cheap deriving (Show,Eq)

instance Monoid Category where
    mempty = Cheap
    mappend NoInline _ = NoInline
    mappend _ NoInline = NoInline
    mappend Strict _ = Strict
    mappend _ Strict = Strict
    mappend Lazy _ = Lazy
    mappend _ Lazy = Lazy
    mappend Cheap Cheap = Cheap

bump :: Category -> Category
bump Cheap = Cheap
bump Lazy = Strict
bump Strict = Strict
bump NoInline = NoInline

funcCategory :: FuncDef -> Category
--funcCategory FuncDef{funcDefBody = Application (Builtin "eval") _}
--    = InlineLazy
funcCategory def = expressionCategory (funcDefBody def)

expressionCategory :: Expression -> Category
expressionCategory (e1 :>>= bind :-> e2)
    = bump (expressionCategory e1) `mappend` expressionCategory e2
expressionCategory (e1 :>> e2)
    = bump (expressionCategory e1) `mappend` expressionCategory e2
expressionCategory (Application fn args) | isExternal fn
    = NoInline
expressionCategory (Application (Builtin "eval") _args)
    = Lazy
expressionCategory (Application (Builtin "apply") _args)
    = Lazy
expressionCategory (Application fn args) | isBuiltin fn
    = Cheap
expressionCategory (Application fn args)
    = Lazy
expressionCategory (Case scrut [_ :> branch])
    = expressionCategory branch
expressionCategory (Case scrut alts)
    = NoInline
expressionCategory (Store (Node _tag ConstructorNode _n _args))
    = Cheap
expressionCategory (Store (Node _tag FunctionNode n _args)) | n >= 1
    = Cheap
expressionCategory Store{}
    = Lazy
expressionCategory Unit{}
    = Cheap




