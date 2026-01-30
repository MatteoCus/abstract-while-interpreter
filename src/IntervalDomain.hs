{-# LANGUAGE InstanceSigs #-}
module IntervalDomain (Interval (..), AbstractDomain (..), Infinitable (..), divide) where

import AbstractDomain (AbstractDomain (..))
import RuntimeConfiguration (RuntimeConfig(..))

-- Definitions for custom interval abstract domain
data Integral a => Infinitable a = Regular a | NegativeInfinity | PositiveInfinity
    deriving Eq

instance (Show a, Integral a) => Show (Infinitable a) where
    show NegativeInfinity = "-inf"
    show PositiveInfinity = "+inf"
    show (Regular x) = show x


data Interval = Empty | Interval (Infinitable Integer) (Infinitable Integer)
    deriving (Show, Eq)

divide :: Infinitable Integer -> Infinitable Integer -> Infinitable Integer
(Regular 0) `divide` (Regular 0) = Regular 0
(Regular x) `divide` (Regular 0)
    | x > 0 = PositiveInfinity
    | otherwise = NegativeInfinity
_ `divide` PositiveInfinity = Regular 0
_ `divide` NegativeInfinity = Regular 0
NegativeInfinity `divide` (Regular x)
    | x >= 0 = NegativeInfinity
    | otherwise = PositiveInfinity
PositiveInfinity `divide` (Regular x)
    | x >= 0 = PositiveInfinity
    | otherwise = NegativeInfinity
(Regular x) `divide` (Regular y) = Regular (x `div` y)

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


instance Ord Interval where
    Empty <= _ = True
    _ <= Empty = False
    (Interval x y) <= (Interval x' y') = x' <= x && y <= y'

instance AbstractDomain Interval where
    (⊥) = Empty
    (⊤) = Interval NegativeInfinity PositiveInfinity

    lub Empty Empty _ = (⊥)
    lub Empty  interval _ =  interval
    lub interval Empty _ =  interval
    lub (Interval mnm1 mxm1) (Interval mnm2 mxm2) config = do
                                                                let (m,n) = intervalBounds config
                                                                if m > n && newMin /= newMax
                                                                then Interval NegativeInfinity PositiveInfinity
                                                                else if newMin == newMax
                                                                    then Interval newMin newMax
                                                                    else
                                                                        case (newMin < m, newMax > n, newMax < m, newMin > n)
                                                                        of  (_,_,True,_) -> Interval NegativeInfinity PositiveInfinity
                                                                            (_,_,_, True) -> Interval NegativeInfinity PositiveInfinity
                                                                            (True, True, _, _) -> Interval NegativeInfinity PositiveInfinity
                                                                            (True, False, _, _) -> Interval NegativeInfinity newMax
                                                                            (False, True,_,_) -> Interval newMin PositiveInfinity
                                                                            (False, False,_,_) -> Interval newMin newMax
                                                                where newMin = min mnm1 mnm2
                                                                      newMax = max mxm1 mxm2

    glb Empty Empty _ = (⊤)
    glb Empty _ _ = (⊥)
    glb _ Empty _ = (⊥)
    glb (Interval mnm1 mxm1) (Interval mnm2 mxm2) config =  if newMin > newMax
                                                            then Empty
                                                            else do
                                                                let (m,n) = intervalBounds config
                                                                if m > n && newMin /= newMax
                                                                then Interval NegativeInfinity PositiveInfinity
                                                                else if newMin == newMax
                                                                    then Interval newMin newMax
                                                                    else
                                                                        case (newMin < m, newMax > n, newMax < m, newMin > n)
                                                                        of  (_,_,True,_) -> Interval NegativeInfinity PositiveInfinity
                                                                            (_,_,_, True) -> Interval NegativeInfinity PositiveInfinity
                                                                            (True, True, _, _) -> Interval NegativeInfinity PositiveInfinity
                                                                            (True, False, _, _) -> Interval NegativeInfinity newMax
                                                                            (False, True,_,_) -> Interval newMin PositiveInfinity
                                                                            (False, False,_,_) -> Interval newMin newMax
                                                            where   newMin = max mnm1 mnm2
                                                                    newMax = min mxm1 mxm2


    Empty + _ = Empty
    _ + Empty = Empty
    (Interval mnm1 mxm1) + (Interval mnm2 mxm2) = Interval (mnm1 Prelude.+ mnm2) (mxm1 Prelude.+ mxm2)

    neg Empty = Empty
    neg (Interval mnm mxm) = Interval (-mxm) (-mnm)

    Empty - _ = Empty
    _ - Empty = Empty
    (Interval mnm1 mxm1) - (Interval mnm2 mxm2) = Interval (mnm1 Prelude.- mxm2) (mxm1 Prelude.- mnm2)

    Empty * _ = Empty
    _ * Empty = Empty
    (Interval mnm1 mxm1) * (Interval mnm2 mxm2) = Interval minim maxim
        where minim = min (min (min (mnm1 Prelude.* mnm2) (mnm1 Prelude.* mxm2)) (mxm1 Prelude.* mnm2)) (mxm1 Prelude.* mxm2)
              maxim = max (max (max (mnm1 Prelude.* mnm2) (mnm1 Prelude.* mxm2)) (mxm1 Prelude.* mnm2)) (mxm1 Prelude.* mxm2)

    (/) Empty _ _= Empty
    (/) _ Empty _ = Empty
    (/) (Interval mnm1 mxm1)  (Interval mnm2 mxm2) config
        | mnm2 >= 1 = Interval (min (mnm1 `divide` mnm2) (mnm1 `divide` mxm2)) (max (mxm1 `divide` mnm2) (mxm1 `divide` mxm2))
        | mxm2 <= -1 = Interval (min (mxm1 `divide` mnm2) (mxm1 `divide` mxm2)) (max (mnm1 `divide` mnm2) (mnm1 `divide` mxm2))
        | otherwise = lub ((AbstractDomain./) (Interval mnm1 mxm1)  (glb (Interval mnm2 mxm2) (Interval (Regular 1) PositiveInfinity) config) config)  ((AbstractDomain./) (Interval mnm1 mxm1)  (glb (Interval mnm2 mxm2) (Interval NegativeInfinity (Regular (-1))) config ) config) config

    Empty ∇ interv = interv
    interv ∇ Empty = interv
    (Interval a b) ∇ (Interval c d) = Interval widenedLower widenedUpper
                                        where widenedLower = if a <= c then a else NegativeInfinity
                                              widenedUpper = if b >= d then b else PositiveInfinity

    Empty △ _ = Empty
    _ △ Empty = Empty
    (Interval a b) △ (Interval c d) = Interval narrowedLower narrowedUpper
                                        where narrowedLower = if a == NegativeInfinity then c else a
                                              narrowedUpper = if b == PositiveInfinity then d else b