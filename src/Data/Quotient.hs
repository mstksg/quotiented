{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Quotient where

import Data.Proxy
import Data.Ord
import Data.Function

newtype a :/ e = Q (EquivClass e a)

instance Equiv e a => Eq (a :/ e) where
    Q x == Q y = eqBy (Proxy :: Proxy e) (Proxy :: Proxy a) x y
    Q x /= Q y = neqBy (Proxy :: Proxy e) (Proxy :: Proxy a) x y

instance (Show a, Equiv e a) => Show (a :/ e) where
    showsPrec p x = showParen (p > 10) $ showString "quotient "
                                       . showsPrec 11 (getCanonical x)

instance (Ord (EquivClass e a), Equiv e a) => Ord (a :/ e) where
    compare = comparing getEquivClass

instance (Num (EquivClass e a), Equiv e a) => Num (a :/ e) where
    (+) = liftQClass2 (+)
    (*) = liftQClass2 (*)
    (-) = liftQClass2 (-)
    negate = liftQClass negate
    abs = liftQClass abs
    signum = liftQClass signum
    fromInteger = withEquivClass . fromInteger

instance (Enum (EquivClass e a), Equiv e a) => Enum (a :/ e) where
    toEnum = withEquivClass . toEnum
    fromEnum = fromEnum . getEquivClass

instance (Real (EquivClass e a), Equiv e a) => Real (a :/ e) where
    toRational = toRational . getEquivClass

instance (Integral (EquivClass e a), Equiv e a) => Integral (a :/ e) where
    quotRem x y = let (d, m) = quotRem (getEquivClass x) (getEquivClass y)
                  in  (withEquivClass d, withEquivClass m)
    toInteger   = toInteger . getEquivClass


class Equiv e a where
    type EquivClass e a

    toEquivClass :: Proxy e -> a -> EquivClass e a
    fromEquivClass :: Proxy e -> EquivClass e a -> a

    eqBy  :: Proxy e -> Proxy a -> EquivClass e a -> EquivClass e a -> Bool
    neqBy :: Proxy e -> Proxy a -> EquivClass e a -> EquivClass e a -> Bool

    default eqBy
        :: Eq (EquivClass e a)
        => Proxy e
        -> Proxy a
        -> EquivClass e a
        -> EquivClass e a
        -> Bool
    eqBy _ _ = (==)

    default neqBy
        :: Eq (EquivClass e a)
        => Proxy e
        -> Proxy a
        -> EquivClass e a
        -> EquivClass e a
        -> Bool
    neqBy _ _ = (/=)

quotient :: forall e a. Equiv e a => a -> a :/ e
quotient = Q . toEquivClass (Proxy :: Proxy e)

getCanonical :: forall e a. Equiv e a => a :/ e -> a
getCanonical (Q c) = fromEquivClass (Proxy :: Proxy e) c

getEquivClass :: a :/ e -> EquivClass e a
getEquivClass (Q c) = c

withEquivClass :: EquivClass e a -> a :/ e
withEquivClass = Q

withEquivClass' :: forall e a. Equiv e a => EquivClass e a -> a :/ e
withEquivClass' x = quotient (fromEquivClass (Proxy :: Proxy e) x :: a)

liftQ
    :: forall e a b. (Equiv e a, Equiv e b)
    => (a -> b)
    -> a :/ e
    -> b :/ e
liftQ f = quotient . f . getCanonical

liftQ2
    :: forall e a b c. (Equiv e a, Equiv e b, Equiv e c)
    => (a -> b -> c)
    -> a :/ e
    -> b :/ e
    -> c :/ e
liftQ2 f x y = quotient $ f (getCanonical x) (getCanonical y)

liftQ3
    :: forall e a b c d. (Equiv e a, Equiv e b, Equiv e c, Equiv e d)
    => (a -> b -> c -> d)
    -> a :/ e
    -> b :/ e
    -> c :/ e
    -> d :/ e
liftQ3 f x y z = quotient $ f (getCanonical x) (getCanonical y) (getCanonical z)

liftQ4
    :: forall e a b c d r. (Equiv e a, Equiv e b, Equiv e c, Equiv e d, Equiv e r)
    => (a -> b -> c -> d -> r)
    -> a :/ e
    -> b :/ e
    -> c :/ e
    -> d :/ e
    -> r :/ e
liftQ4 f x y z a = quotient $ f (getCanonical x) (getCanonical y) (getCanonical z) (getCanonical a)

liftQClass
    :: forall e f a b. (Equiv e a, Equiv f b)
    => (EquivClass e a -> EquivClass f b)
    -> a :/ e
    -> b :/ f
liftQClass f (Q x) = Q $ f x

liftQClass2
    :: forall e f g a b c. (Equiv e a, Equiv f b, Equiv g c)
    => (EquivClass e a -> EquivClass f b -> EquivClass g c)
    -> a :/ e
    -> b :/ f
    -> c :/ g
liftQClass2 f (Q x) (Q y) = Q $ f x y

liftQClass3
    :: forall e f g h a b c d. (Equiv e a, Equiv f b, Equiv g c, Equiv h d)
    => (EquivClass e a -> EquivClass f b -> EquivClass g c -> EquivClass h d)
    -> a :/ e
    -> b :/ f
    -> c :/ g
    -> d :/ h
liftQClass3 f (Q x) (Q y) (Q z) = Q $ f x y z

liftQClass4
    :: forall e f g h j a b c d r. (Equiv e a, Equiv f b, Equiv g c, Equiv h d, Equiv j r)
    => (EquivClass e a -> EquivClass f b -> EquivClass g c -> EquivClass h d -> EquivClass j r)
    -> a :/ e
    -> b :/ f
    -> c :/ g
    -> d :/ h
    -> r :/ j
liftQClass4 f (Q x) (Q y) (Q z) (Q a) = Q $ f x y z a

