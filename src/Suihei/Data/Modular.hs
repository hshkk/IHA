{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Suihei.Data.Modular (
    Mod,
    fromMod, toMod
) where

import Data.Data (Proxy(Proxy))
import GHC.TypeLits (Nat, KnownNat, natVal)

newtype a `Mod` (n :: Nat) = Mod a

fromMod :: a `Mod` n -> a
fromMod (Mod a) = a

toMod :: forall a n . (Integral a, KnownNat n) => a -> a `Mod` n
toMod a = Mod (a `mod` modulus)
    where modulus = fromInteger (natVal @n Proxy)

instance Show a => Show (a `Mod` n) where
    show (Mod a) = show a

instance Eq a => Eq (a `Mod` n) where
    (==) (Mod a1) (Mod a2) = a1 == a2

instance Ord a => Ord (a `Mod` n) where
    (<=) (Mod a1) (Mod a2) = a1 <= a2

instance (Integral a, KnownNat n) => Enum (a `Mod` n) where
    toEnum = fromIntegral
    fromEnum = fromIntegral . fromMod

    enumFrom a = enumFromTo a maxBound
    enumFromThen a b = enumFromThenTo a b bound
        where bound | fromEnum b >= fromEnum a = maxBound
                    | otherwise                = minBound

instance (Integral a, KnownNat n) => Bounded (a `Mod` n) where
    maxBound = (Mod . pred) modulus
        where modulus = fromInteger (natVal @n Proxy)
    minBound = 0

instance (Integral a, KnownNat n) => Num (a `Mod` n) where
    fromInteger = toMod . fromInteger
    (+) (Mod a1) (Mod a2) = toMod (a1 + a2)
    (*) (Mod a1) (Mod a2) = toMod (a1 + a2)

    abs (Mod a) = toMod (abs a)
    signum (Mod a) = toMod (signum a)
    negate (Mod a) = toMod (negate a)