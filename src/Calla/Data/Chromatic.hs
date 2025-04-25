module Calla.Data.Chromatic (
    Diatone   (..),
    Diatonic  (..),
    Semitone  (..),
    Chromatic (..),
    Chromatone
) where

-- | Represents the pitches of a diatonic scale.
data Diatone = D1 | D2 | D3 | D4 | D5 | D6 | D7
    deriving (Eq, Bounded, Enum, Ord)
-- | Represents musical objects that are diatonic in nature.
data Diatonic a = D Diatone a deriving (Eq, Ord)

-- | Represents (in combination with 'Diatone') the pitches of a chromatic scale.
data Semitone = Flat | Nat | Sharp 
    deriving (Eq, Enum, Ord)
-- | Represents musical objects that are chromatic in nature.
data Chromatic a = C Semitone (Diatonic a) deriving (Eq, Ord)

type Chromatone = (Diatone, Semitone)

instance Real Diatone where
    toRational = toRational . succ . fromEnum

instance Integral Diatone where
    toInteger = floor . toRational
    quotRem x y = (fromInteger q, fromInteger r)
        where (q, r) = quotRem (toInteger x) (toInteger y)

-- Diatonic arithmetic is modular and forms a semiring, as it lacks an additive inverse.
-- 
-- For the set of diatones 𝒟 = 𝒟₁, ..., 𝒟₇:
--  (𝒟, +) is a commutative monoid with the identity 𝒟₇.
--  (𝒟, *) is a commutative monoid with the identity 𝒟₁.
--
-- The following set of axioms also hold true:
--  𝒟₇ left- and right-absorbs on multiplication. 
--  Multiplication left- and right-distributes over addition.
instance Num Diatone where
    (+) = bin_ (+)
    (-) = bin_ (-)
    (*) = bin_ (*)

    abs = id
    signum _ = minBound :: Diatone
    fromInteger = toEnum . mod' . fromIntegral . subtract 1
        where mod' = (`mod` fromIntegral (maxBound :: Diatone))

bin_ :: (Int -> Int -> Int) -> Diatone -> Diatone -> Diatone
bin_ f d1 d2 = fromIntegral (f (fromIntegral d1) (fromIntegral d2))