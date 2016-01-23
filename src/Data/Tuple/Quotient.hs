{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Tuple.Quotient where

import Data.Proxy
import Data.Quotient
import Data.List

data Unordered

instance Ord a => Equiv Unordered (a, a) where
    type EquivClass Unordered (a, a) = (a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a) :/ Unordered))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y)
      | x <= y    = (x, y)
      | otherwise = (y, x)

instance Ord a => Equiv Unordered (a, a, a) where
    type EquivClass Unordered (a, a, a) = (a, a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a, a) :/ Unordered))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y, z)
      = case sort [x,y,z] of
          [x',y',z'] -> (x', y', z')
          _          -> error "Unexpected result of sort"

instance Ord a => Equiv Unordered (a, a, a, a) where
    type EquivClass Unordered (a, a, a, a) = (a, a, a, a)

    toEquivClass _ = normalizeEquivClass (Proxy :: Proxy ((a, a, a, a) :/ Unordered))
    fromEquivClass _ = id

    normalizeEquivClass _ (x, y, z, r)
      = case sort [x,y,z,r] of
          [x',y',z',r'] -> (x', y', z', r')
          _             -> error "Unexpected result of sort"
