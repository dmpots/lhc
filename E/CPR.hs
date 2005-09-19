module E.CPR(Val(..), cprAnalyzeBinds, cprAnalyze) where

import Control.Monad.Writer
import Data.Generics
import Data.Monoid()
import Doc.DocLike
import E.E
import E.FreeVars
import Name
import qualified Data.Map as Map
import qualified Doc.Chars as C
import qualified Info

newtype Env = Env (Map.Map TVr Val)
    deriving(Monoid)

data Val =
    Top           -- the top.
    | Fun Val     -- function taking an arg
    | Tup Name    -- A constructed product
    | Tag [Name]  -- A nullary constructor, like True, False
    | Bot         -- the bottom
    deriving(Eq,Ord,Typeable)

instance Show Val where
    showsPrec _ Top = C.top
    showsPrec _ Bot = C.bot
    showsPrec n (Fun v) = C.lambda <> showsPrec n v
    showsPrec _ (Tup n) = shows n
    -- showsPrec _ (Tag [n]) = shows n
    showsPrec _ (Tag ns) = shows ns

lub :: Val -> Val -> Val
lub Bot a = a
lub a Bot = a
lub Top a = Top
lub a Top = Top
lub (Tup a) (Tup b)
    | a == b = Tup a
    | otherwise = Top
lub (Fun l) (Fun r) = Fun (lub l r)
lub (Tag xs) (Tag ys) = Tag (smerge xs ys)
lub (Tag _) (Tup _) = Top
lub (Tup _) (Tag _) = Top
lub a b = error $ "CPR.lub: " ++ show (a,b)


instance Monoid Val where
    mempty = Bot
    mappend = lub


smerge :: Ord a => [a] -> [a] -> [a]
smerge (x:xs) (y:ys)
    | x == y = x:smerge xs ys
    | x < y = x:smerge xs (y:ys)
    | otherwise = y:smerge (x:xs) ys
smerge [] ys = ys
smerge xs [] = xs

cprAnalyzeBinds :: Env -> [(TVr,E)] -> ([(TVr,E)],Env)
cprAnalyzeBinds env bs = f env  (decomposeDefns bs) [] where
    f env (Left (t,e):rs) zs = case cprAnalyze env e of
        (e',v) -> f (envInsert t v env) rs ((tvrInfo_u (Info.insert v) t,e'):zs)
    f env (Right xs:rs) zs = g (length xs + 2) ([ (t,(e,Bot)) | (t,e) <- xs]) where
        g 0 mp =  f nenv rs ([ (tvrInfo_u (Info.insert b) t,e)   | (t,(e,b)) <- mp] ++ zs)  where
            nenv = Env (Map.fromList [ (t,b) | (t,(e,b)) <- mp]) `mappend` env
        g n mp = g (n - 1) [ (t,cprAnalyze nenv e)  | (t,e) <- xs] where
            nenv = Env (Map.fromList [ (t,b) | (t,(e,b)) <- mp]) `mappend` env
    f env [] zs = (reverse zs,env)


envInsert :: TVr -> Val -> Env -> Env
envInsert tvr val (Env mp) = Env $ Map.insert tvr val mp

cprAnalyze :: Env -> E -> (E,Val)
cprAnalyze (Env mp) (EVar v)
    | Just t <- Map.lookup v mp = (EVar v,t)
    | Just t <- Info.lookup (tvrInfo v)  = (EVar v,t)
    | otherwise = (EVar v,Top)
cprAnalyze env (ELetRec ds e) = (ELetRec ds' e',val) where
    (ds',env') = cprAnalyzeBinds env ds
    (e',val) = cprAnalyze (env' `mappend` env) e
cprAnalyze env (ELam t e) = (ELam t e',Fun val) where
    (e',val) = cprAnalyze (envInsert t Top env) e
cprAnalyze env ec@(ECase {}) = runWriter (caseBodiesMapM f ec) where
    f e = do
        (e',v) <- return $ cprAnalyze env e
        tell v
        return e'
cprAnalyze env (EAp fun arg) = (EAp fun_cpr arg,res_res) where
    (fun_cpr, fun_res) = cprAnalyze env fun
    res_res = case fun_res of
        Fun x -> x
        Top -> Top
        Bot -> Bot
        v -> error $ "cprAnalyze.res_res: " ++ show v
cprAnalyze env  e = (e,f e) where
    f (ELit (LitInt {})) = Top
    f (ELit (LitCons n [] _)) = Tag [n]
    f (ELit (LitCons n _  _)) = Tup n
    f (EPi _ _) = Tup tArrow
    f (EPrim {}) = Top -- TODO fix primitives
    f (EError {}) = Bot
    f e = error $ "cprAnalyze.f: " ++ show e
    {-
    f (ELam t e) = Fun (cprAnalyze (Env $ Map.insert t Top mp)  e)
    f (EVar v)
        | Just v <- Map.lookup v mp = v
        | otherwise = Top
     -}




