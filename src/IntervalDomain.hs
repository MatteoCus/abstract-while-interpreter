{-# LANGUAGE InstanceSigs #-}
module IntervalDomain (Interval (..), AbstractDomain (..), Infinitable (..), divide) where

import AbstractDomain (AbstractDomain (..))
import RuntimeConfiguration (RuntimeConfig(..))
import Infinitable (Infinitable (..))

-- Interval type definition
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
                                        where widenedLower
                                                | a <= c = a
                                                | 0 <= c && c < a = 0
                                                | otherwise = NegativeInfinity
                                              widenedUpper
                                                | b >= d = b
                                                | 0 >= d && d > b = 0
                                                | otherwise = PositiveInfinity

    Empty △ _ = Empty
    _ △ Empty = Empty
    (Interval a b) △ (Interval c d) = Interval narrowedLower narrowedUpper
                                        where narrowedLower = if a == NegativeInfinity then c else a
                                              narrowedUpper = if b == PositiveInfinity then d else b