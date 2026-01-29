module AbstractDomain (AbstractDomain(..)) where
import RuntimeConfiguration (RuntimeConfig)

infixl 6 +, -, ∇, △
infixl 7 *, /

-- General AbstractDomain class
class Ord d => AbstractDomain d where
    (⊥) :: d
    (⊤) :: d
    lub :: d -> d -> RuntimeConfig -> d
    glb :: d -> d -> RuntimeConfig -> d
    (+) :: d -> d -> d
    neg :: d -> d
    (-) :: d -> d -> d
    (*) :: d -> d -> d
    (/) :: d -> d -> RuntimeConfig -> d
    (∇) :: d -> d -> d
    (△) :: d -> d -> d 