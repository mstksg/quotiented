{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Quotient where

import Data.Finite
import Numeric.Natural
import Data.Proxy
import GHC.TypeLits
import Data.Quotient

data Mod :: Nat -> *

instance (KnownNat m, Integral n) => Equiv (Mod m) n where
    type EquivClass (Mod m) n = Finite m
    toEquivClass _ = finite . (`mod` natVal (Proxy :: Proxy m))
                   . fromIntegral
    fromEquivClass _ = fromInteger . getFinite

-- data Diff

-- instance Equiv Diff (Natural, Natural) where
--     type EquivClass Diff (a, a) = (Natural, Natural)
--     -- toEquivClass _ (x, y) = (x - y, 0)
--     -- fromEquivClass _ (x, y) = 


