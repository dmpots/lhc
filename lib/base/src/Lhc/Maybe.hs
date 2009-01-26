{-# OPTIONS_LHC -N -fffi #-}
module Lhc.Maybe where

import Lhc.Monad
import Lhc.Order
import Lhc.Show
import Lhc.List
import Lhc.Basics
import Lhc.Num

instance Monad Maybe where
    return x = Just x
    Nothing >>= _ = Nothing
    Just x >>= y = y x
    fail _ = Nothing



instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)



-- Maybe
-- need to add Read instance

data Maybe a  =  Nothing | Just a
    deriving (Eq, Ord, Show)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f m = case m of
    Just x -> f x
    Nothing -> n


