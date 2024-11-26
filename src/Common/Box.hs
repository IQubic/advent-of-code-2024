{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Box ( Box(..)
                  , Box'
                  , Nat(..)
                  , size
                  , intersection
                  , intersections
                  , remove
                  , union
                  , unions
                  ) where

import GHC.List (foldl1')
import Control.Monad (guard, foldM)

import Data.Kind (Type)
import GHC.Stack.Types (HasCallStack)
import GHC.TypeNats qualified as T

-- | Natural numbers (used for type index)
data Nat = Z | S Nat

-- | Covert from GHC type literal syntax to an inductively defined natural
type family FromNatural (n :: T.Natural) :: Nat where
  FromNatural 0 = 'Z
  FromNatural n = 'S (FromNatural (n T.- 1))

-- | Type synonym for 'Box' allowing the use of natural number literals.
type Box' n = Box (FromNatural n)

-- | An n-dimensional box
-- | Invariant: lower bound must actually be lower
data Box :: Nat -> Type where
  Pt  ::  Box Z -- ^ A single point
  Dim ::  !Int {- ^ inclusive lower bound -} ->
          !Int {- ^ exclusive upper bound -} ->
          Box n {- ^ lower dimensional box -} ->
          Box ('S n) -- ^ A box extended along an axis
deriving instance Show (Box n)
deriving instance Eq (Box n)
deriving instance Ord (Box n)

-- | Gets the size of a box
size :: Box n -> Int
size Pt              = 1
size (Dim lo hi box) = (hi - lo) * size box

-- | Intersection of two boxes
intersection :: Box n -> Box n -> Maybe (Box n)
intersection Pt Pt = Just Pt
intersection (Dim a b xs) (Dim c d ys) = do
  let x = max a c
  let y = min b d
  guard $ x < y
  zs <- intersection xs ys
  Just $ Dim x y zs

-- | Intersection of one or more boxes.
intersections :: HasCallStack => [Box n] -> Maybe (Box n)
intersections []     = error "intersection: empty intersection"
intersections (x:xs) = foldM intersection x xs

remove ::
  Box n {- ^ from this -} ->
  Box n {- ^ remove this -} ->
  [Box n] {- ^ leaving these -}
remove b1 b2 =
  case intersection b1 b2 of
    Nothing -> [b1]
    Just b  -> b1 `remove'` b

-- | Worker for 'remove' where the first argument is a
-- subset of the second argument.
remove' :: Box n -> Box n -> [Box n]
remove' Pt Pt = []
remove' (Dim a b xs) (Dim c d ys) =
  [Dim a c xs | a < c] ++
  [Dim c d zs | zs <- remove' xs ys] ++
  [Dim d b xs | d < b]

-- | Compute the box that encompasses both arguments.
union :: Box n -> Box n -> Box n
union (Dim a b xs) (Dim c d ys) = Dim (min a c) (max b d) (xs `union` ys)
union Pt Pt = Pt

-- | Compute the box that encompasses all of the boxes in the list.
unions :: [Box n] -> Box n
unions = foldl1' union
