{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Tuple.Quotient where

import Data.Proxy
import Data.Quotient
import Data.List

data Permutation

instance Ord a => Equiv Permutation (a, a) where
    type EquivClass Permutation (a, a) = (a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a) :/ Permutation))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y)
      | x <= y    = (x, y)
      | otherwise = (y, x)

instance Ord a => Equiv Permutation (a, a, a) where
    type EquivClass Permutation (a, a, a) = (a, a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a, a) :/ Permutation))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y, z)
      = case sort [x,y,z] of
          [x',y',z'] -> (x', y', z')
          _          -> error "Unexpected result of sort"

instance Ord a => Equiv Permutation (a, a, a, a) where
    type EquivClass Permutation (a, a, a, a) = (a, a, a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a, a, a) :/ Permutation))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y, z, r)
      = case sort [x,y,z,r] of
          [x',y',z',r'] -> (x', y', z', r')
          _             -> error "Unexpected result of sort"

data Cycle

instance Ord a => Equiv Cycle (a, a) where
    type EquivClass Cycle (a, a) = (a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a) :/ Cycle))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y)
      | x <= y    = (x, y)
      | otherwise = (y, x)

instance Ord a => Equiv Cycle (a, a, a) where
    type EquivClass Cycle (a, a, a) = (a, a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a, a) :/ Cycle))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y, z)
      | x <= y && x <= z = (x, y, z)
      | y <= x && y <= z = (y, z, x)
      | otherwise        = (z, x, y)

instance Ord a => Equiv Cycle (a, a, a, a) where
    type EquivClass Cycle (a, a, a, a) = (a, a, a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a, a, a) :/ Cycle))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y, z, r)
      | x <= y && x <= z && x <= r = (x, y, z, r)
      | y <= x && y <= z && y <= r = (y, z, r, x)
      | z <= x && z <= y && z <= r = (z, r, x, y)
      | otherwise                  = (r, x, y, z)
