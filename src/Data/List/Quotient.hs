{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.List.Quotient where

import Data.Quotient
import qualified Data.Set as S

data Elems

instance Ord a => Equiv Elems [a] where
    type EquivClass Elems [a] = S.Set a
    toEquivClass _ = S.fromList
    fromEquivClass _ = S.toList


