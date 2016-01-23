{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Quotient where

import Data.Finite
import Data.Function
import Data.Ord
import Data.Proxy
import Data.Quotient
import GHC.TypeLits
import Numeric.Natural

data Mod :: Nat -> *

instance (KnownNat m, Integral n) => Equiv (Mod m) n where
    type EquivClass (Mod m) n = Finite m
    toEquivClass _ = finite . (`mod` natVal (Proxy :: Proxy m))
                   . fromIntegral
    fromEquivClass _ = fromInteger . getFinite

data Diff

data Natural2 = N2 Natural Natural
    deriving (Show, Eq, Read)

instance Equiv Diff Natural2 where
    type EquivClass Diff Natural2 = Integer
    toEquivClass _ (N2 x y) = fromIntegral x - fromIntegral y
    fromEquivClass _ n
        | n >= 0    = N2 (fromIntegral n) 0
        | otherwise = N2 0 (fromIntegral (negate n))

