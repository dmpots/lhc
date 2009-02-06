
-- | similar to GenUtil but can rely on non-haskell 98 features
module Util.Gen(module Util.Gen, module GenUtil) where

import Control.Monad.Writer
import Control.Monad.Identity
import Data.Monoid
import Data.List

import GenUtil hiding(replicateM)

mconcatMap :: Monoid b => (a -> b) -> [a] -> b
mconcatMap f xs = mconcat (map f xs)
mintercalate :: Monoid a => a -> [a] -> a
mintercalate x xs = mconcat (intersperse x xs)

mconcatMapM :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
mconcatMapM f xs = mapM f xs >>= return . mconcat


runEither :: String -> Either String a -> a
runEither msg (Left fm) = error $ msg ++ " - " ++ fm
runEither _ (Right a) = a

travCollect :: Monoid w => ((a -> Writer w a) -> a -> Writer w a) -> (a -> w) -> a -> w
travCollect fn col x = execWriter (f x) where
    f x = tell (col x) >> fn f x

forMn_ :: Monad m => [a] -> ((a, Int) -> m b) -> m ()
forMn_ xs = forM_ (zip xs [0 :: Int .. ])
forMn :: Monad m => [a] -> ((a, Int) -> m b) -> m [b]
forMn xs = forM (zip xs [0 :: Int .. ])
