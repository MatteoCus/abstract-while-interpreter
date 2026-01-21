module AbstractDomain (AbstractDomain(..)) where

-- General AbstractDomain class
class Ord d => AbstractDomain d where
    (⊥) :: d
    (⊤) :: d
    lub :: [d] -> d
    glb :: [d] -> d
    (+) :: d -> d -> d
    neg :: d -> d
    (-) :: d -> d -> d
    (*) :: d -> d -> d
    (/) :: d -> d -> d