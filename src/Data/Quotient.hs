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
    Q x == Q y = eqBy (Proxy :: Proxy (a :/ e)) x y
    Q x /= Q y = neqBy (Proxy :: Proxy (a :/ e)) x y

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

instance (Fractional (EquivClass e a), Equiv e a) => Fractional (a :/ e) where
    (/) = liftQClass2 (/)
    recip = liftQClass recip
    fromRational = withEquivClass . fromRational

instance (RealFrac (EquivClass e a), Equiv e a) => RealFrac (a :/ e) where
    properFraction x = let (n, d) = properFraction (getEquivClass x)
                       in  (n, withEquivClass d)

instance (Floating (EquivClass e a), Equiv e a) => Floating (a :/ e) where
    pi = withEquivClass pi
    exp = liftQClass exp
    log = liftQClass log
    sin = liftQClass sin
    cos = liftQClass cos
    asin = liftQClass asin
    acos = liftQClass acos
    atan = liftQClass atan
    sinh = liftQClass sinh
    cosh = liftQClass cosh
    asinh = liftQClass asinh
    acosh = liftQClass acosh
    atanh = liftQClass atanh

instance (RealFloat (EquivClass e a), Equiv e a) => RealFloat (a :/ e) where
    floatRadix = floatRadix . getEquivClass
    floatDigits = floatDigits . getEquivClass
    floatRange = floatRange . getEquivClass
    decodeFloat = decodeFloat . getEquivClass
    encodeFloat i = withEquivClass . encodeFloat i
    exponent = exponent . getEquivClass
    significand = liftQClass significand
    scaleFloat s = liftQClass (scaleFloat s)
    isNaN = isNaN . getEquivClass
    isInfinite = isInfinite . getEquivClass
    isDenormalized = isDenormalized . getEquivClass
    isNegativeZero = isNegativeZero . getEquivClass
    isIEEE = isIEEE . getEquivClass
    atan2 = liftQClass2 atan2

instance (Monoid (EquivClass e a), Equiv e a) => Monoid (a :/ e) where
    mempty = withEquivClass mempty
    mappend = liftQClass2 mappend


class Equiv e a where
    type EquivClass e a

    toEquivClass :: Proxy e -> a -> EquivClass e a
    fromEquivClass :: Proxy e -> EquivClass e a -> a

    normalizeEquivClass :: Proxy (a :/ e) -> EquivClass e a -> EquivClass e a
    normalizeEquivClass _ = id
    eqBy  :: Proxy (a :/ e) -> EquivClass e a -> EquivClass e a -> Bool
    neqBy :: Proxy (a :/ e) -> EquivClass e a -> EquivClass e a -> Bool

    default eqBy
        :: Eq (EquivClass e a)
        => Proxy (a :/ e)
        -> EquivClass e a
        -> EquivClass e a
        -> Bool
    eqBy _ = (==)

    default neqBy
        :: Eq (EquivClass e a)
        => Proxy (a :/ e)
        -> EquivClass e a
        -> EquivClass e a
        -> Bool
    neqBy _ = (/=)

quotient :: forall e a. Equiv e a => a -> a :/ e
quotient = Q . toEquivClass (Proxy :: Proxy e)

getCanonical :: forall e a. Equiv e a => a :/ e -> a
getCanonical (Q c) = fromEquivClass (Proxy :: Proxy e) c

getEquivClass :: a :/ e -> EquivClass e a
getEquivClass (Q c) = c

withEquivClass :: forall e a. Equiv e a => EquivClass e a -> a :/ e
withEquivClass = Q . normalizeEquivClass (Proxy :: Proxy (a :/ e))

unsafeWithEquivClass :: EquivClass e a -> a :/ e
unsafeWithEquivClass = Q


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
liftQClass f (Q x) = withEquivClass $ f x

liftQClass2
    :: forall e f g a b c. (Equiv e a, Equiv f b, Equiv g c)
    => (EquivClass e a -> EquivClass f b -> EquivClass g c)
    -> a :/ e
    -> b :/ f
    -> c :/ g
liftQClass2 f (Q x) (Q y) = withEquivClass $ f x y

liftQClass3
    :: forall e f g h a b c d. (Equiv e a, Equiv f b, Equiv g c, Equiv h d)
    => (EquivClass e a -> EquivClass f b -> EquivClass g c -> EquivClass h d)
    -> a :/ e
    -> b :/ f
    -> c :/ g
    -> d :/ h
liftQClass3 f (Q x) (Q y) (Q z) = withEquivClass $ f x y z

liftQClass4
    :: forall e f g h j a b c d r. (Equiv e a, Equiv f b, Equiv g c, Equiv h d, Equiv j r)
    => (EquivClass e a -> EquivClass f b -> EquivClass g c -> EquivClass h d -> EquivClass j r)
    -> a :/ e
    -> b :/ f
    -> c :/ g
    -> d :/ h
    -> r :/ j
liftQClass4 f (Q x) (Q y) (Q z) (Q a) = withEquivClass $ f x y z a

