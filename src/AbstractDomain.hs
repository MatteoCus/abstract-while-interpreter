module AbstractDomain (AbstractDomain(..)) where

infixl 6 +, -, ∇, △
infixl 7 *, /

-- General AbstractDomain class
class Ord d => AbstractDomain d where
    (⊥) :: d
    (⊤) :: d
    lub :: d -> d -> d
    glb :: d -> d -> d
    (+) :: d -> d -> d
    neg :: d -> d
    (-) :: d -> d -> d
    (*) :: d -> d -> d
    (/) :: d -> d -> d
    (∇) :: d -> d -> d
    (△) :: d -> d -> d 