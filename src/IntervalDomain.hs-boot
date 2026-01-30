module IntervalDomain where
data Integral a => Infinitable a = Regular a | NegativeInfinity | PositiveInfinity

instance (Show a, Integral a) => Show (Infinitable a)