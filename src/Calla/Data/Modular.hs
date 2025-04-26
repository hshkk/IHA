{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calla.Data.Modular (
    Mod, type (%%),
    fromMod, toMod
) where

import Data.Data (Proxy(Proxy))
import GHC.TypeLits (Nat, KnownNat, natVal)

newtype a `Mod` (n :: Nat) = Mod a
type (%%) = Mod

fromMod :: a %% n -> a
fromMod (Mod a) = a

toMod :: forall a n . (Integral a, KnownNat n) => a -> a %% n
toMod a = Mod (a `mod` modulus)
    where modulus = fromInteger (natVal @n Proxy)

instance (Integral a, KnownNat n) => Num (a %% n) where
    fromInteger = toMod . fromInteger
    (+) (Mod a1) (Mod a2) = toMod (a1 + a2)
    (*) (Mod a1) (Mod a2) = toMod (a1 + a2)

    abs (Mod a) = toMod (abs a)
    signum (Mod a) = toMod (signum a)
    negate (Mod a) = toMod (negate a)

instance (Integral a, KnownNat n) => Enum (a %% n) where
    toEnum = fromIntegral
    fromEnum = fromIntegral . fromMod

    enumFrom a = enumFromTo a maxBound
    enumFromThen a b = enumFromThenTo a b bound
        where bound | fromEnum b >= fromEnum a = maxBound
                    | otherwise                = minBound

instance (Integral a, KnownNat n) => Bounded (a %% n) where
    maxBound = (Mod . pred) (fromInteger (natVal @n Proxy))
    minBound = 0