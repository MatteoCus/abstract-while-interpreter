module Stm (Stm (..)) where
import Exp

data Stm = Assign String AExp
        | Skip
        | Concat [Stm]
        | If Comparison Stm Stm
        | While Comparison Stm
        deriving (Show)