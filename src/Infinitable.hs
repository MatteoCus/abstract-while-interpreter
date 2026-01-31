module Infinitable( Infinitable(..)) where
data Integral a => Infinitable a = Regular a | NegativeInfinity | PositiveInfinity
    deriving Eq

instance (Show a, Integral a) => Show (Infinitable a) where
    show NegativeInfinity = "-inf"
    show PositiveInfinity = "+inf"
    show (Regular x) = show x

instance (Integral a) => Num (Infinitable a) where
    NegativeInfinity + _ = NegativeInfinity
    _ + NegativeInfinity = NegativeInfinity
    PositiveInfinity + _ = PositiveInfinity
    _ + PositiveInfinity = PositiveInfinity
    (Regular a) + (Regular b) = Regular (a Prelude.+ b)

    NegativeInfinity * PositiveInfinity = NegativeInfinity
    NegativeInfinity * NegativeInfinity = PositiveInfinity
    PositiveInfinity * PositiveInfinity = PositiveInfinity
    PositiveInfinity * NegativeInfinity = NegativeInfinity
    NegativeInfinity * (Regular a)
        | a < 0 = PositiveInfinity
        | a == 0 = Regular 0
        | otherwise = NegativeInfinity
    PositiveInfinity * (Regular a)
        | a < 0 = NegativeInfinity
        | a == 0 = Regular 0
        | otherwise = PositiveInfinity
    (Regular a) * NegativeInfinity
        | a < 0 = PositiveInfinity
        | a == 0 = Regular 0
        | otherwise = NegativeInfinity
    (Regular a) * PositiveInfinity
        | a < 0 = NegativeInfinity
        | a == 0 = Regular 0
        | otherwise = PositiveInfinity
    (Regular a) * (Regular b) = Regular (a Prelude.* b)

    abs NegativeInfinity = PositiveInfinity
    abs PositiveInfinity = PositiveInfinity
    abs (Regular a) = Regular (abs a)

    signum NegativeInfinity = Regular (-1)
    signum PositiveInfinity = Regular 1
    signum (Regular a) = Regular (signum a)

    fromInteger n = Regular (fromIntegral n)

    negate NegativeInfinity = PositiveInfinity
    negate PositiveInfinity = NegativeInfinity
    negate (Regular a) = Regular (-a)



instance (Integral a) => Ord (Infinitable a) where
    compare NegativeInfinity NegativeInfinity = EQ
    compare PositiveInfinity PositiveInfinity = EQ
    compare NegativeInfinity _ = LT
    compare PositiveInfinity _ = GT
    compare _ PositiveInfinity = LT
    compare _ NegativeInfinity = GT
    compare (Regular x) (Regular y) = compare x y