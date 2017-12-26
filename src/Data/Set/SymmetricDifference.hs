{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Set.SymmetricDifference (SymmetricDifference) where

import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Group (Group(..), Abelian)
import Data.Set (Set, union, intersection, (\\), empty)

-- Symmetric Difference group on sets

newtype SymmetricDifference a = SymmetricDifference (Set a)
  deriving (Show, Eq, Ord, Eq1, Ord1, Show1)

instance Ord a => Monoid (SymmetricDifference a) where
  mempty = SymmetricDifference empty
  mappend (SymmetricDifference xs) (SymmetricDifference ys) =
    SymmetricDifference $ (xs `union` ys) \\ (xs `intersection` ys)

instance Ord a => Group (SymmetricDifference a) where
  invert = id
  -- O(1). Default implementation is O(log_2(n))
  pow x0 n0 = case compare n0 0 of
    LT -> invert . f x0 $ negate n0
    EQ -> mempty
    GT -> f x0 n0
    where f x n = if even n then mempty else x

instance Ord a => Abelian (SymmetricDifference a)
