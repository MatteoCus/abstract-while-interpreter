module PrettyPrint where
import {-# SOURCE #-} IntervalDomain (Interval)
import Infinitable (Infinitable)
import AbstractInterpreter.State (State)

-- | Pretty print an interval
prettyInterval :: Interval -> String

-- | Pretty print an infinitable value
prettyInfinitable :: Infinitable Integer -> String

-- | Pretty print a range (for constants)
prettyRange :: Infinitable Integer -> Infinitable Integer -> String

-- | Pretty print a state
prettyState :: State -> String