module FrontEnd.SrcLoc where

import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Foldable

import Data.DeriveTH
import Data.Derive.All
import Data.Monoid
import Data.Generics
import Data.Binary


data SrcLoc = SrcLoc { srcLocFileName :: String, srcLocLine :: !Int, srcLocColumn :: !Int}
    deriving(Data,Typeable,Eq,Ord)
$(derive makeBinary ''SrcLoc)
$(derive makeUpdate ''SrcLoc)

data SrcSpan = SrcSpan { srcSpanBegin :: !SrcLoc, srcSpanEnd :: !SrcLoc }
    deriving(Data,Typeable,Eq,Ord)
$(derive makeUpdate ''SrcSpan)

bogusASrcLoc :: SrcLoc
bogusASrcLoc = SrcLoc "bogus#" (-1) (-1)

bogusSrcSpan :: SrcSpan
bogusSrcSpan = SrcSpan bogusASrcLoc bogusASrcLoc

instance Monoid SrcLoc where
    mempty = bogusASrcLoc
    mappend a b
        | a == bogusASrcLoc = b
        | otherwise = a

--------------------
-- haslocation class
--------------------

class HasLocation a where
    srcLoc :: a -> SrcLoc
    srcSpan :: a -> SrcSpan
    srcSpan x = bogusSrcSpan { srcSpanBegin = slx, srcSpanEnd = slx } where slx = srcLoc x
    srcLoc x = srcSpanBegin (srcSpan x)

instance HasLocation a => HasLocation [a] where
    srcLoc xs = mconcat (map srcLoc xs)

instance HasLocation SrcLoc where
    srcLoc x = x

instance HasLocation SrcSpan where
    srcSpan x = x

instance HasLocation (SrcLoc,SrcLoc) where
    srcSpan (x,y) = SrcSpan x y

instance HasLocation (Located a) where
    srcSpan (Located x _) = x

data Located x = Located SrcSpan x
    deriving(Ord,Show,Data,Typeable,Eq)

fromLocated :: Located x -> x
fromLocated (Located _ x) = x

instance Functor Located where
    fmap f (Located l x) = Located l (f x)

instance Foldable Located where
    foldMap f (Located l x) = f x

instance Traversable Located where
    traverse f (Located l x) = Located l <$> f x

located :: HasLocation a => a -> b -> Located b
located ss x = Located (srcSpan ss) x


-----------------------
-- srcloc monad classes
-----------------------

class Monad m => MonadSrcLoc m where
    getSrcLoc  :: m SrcLoc
    getSrcSpan :: m SrcSpan
    getSrcSpan = getSrcLoc >>= return . srcSpan
    getSrcLoc = getSrcSpan >>= return . srcLoc


class MonadSrcLoc m => MonadSetSrcLoc m where
    withSrcLoc :: SrcLoc -> m a -> m a
    withSrcSpan :: SrcSpan -> m a -> m a
    withSrcLoc sl a = withSrcSpan (srcSpan sl) a
    withSrcSpan ss a = withSrcLoc (srcLoc ss) a

withLocation :: (HasLocation l,MonadSetSrcLoc m) => l -> m a -> m a
withLocation l = withSrcSpan (srcSpan l)

instance Monoid w => MonadSrcLoc (Writer w) where
    getSrcLoc = return mempty
instance Monoid w => MonadSetSrcLoc (Writer w) where
    withSrcLoc _ a = a

instance MonadSrcLoc Identity where
    getSrcLoc = return mempty
instance MonadSetSrcLoc Identity where
    withSrcLoc _ a = a

-----------------
-- show instances
-----------------

instance Show SrcLoc where
    show (SrcLoc fn l c) = fn ++ f l ++ f c where
        f (-1) = ""
        f n = ':':show n

instance Show SrcSpan where
    show SrcSpan { srcSpanBegin =  sl1, srcSpanEnd = sl2 }
      | sl1 == sl2 = show sl1
      | otherwise = show sl1 ++ "-" ++ show sl2
