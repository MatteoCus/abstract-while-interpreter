module Stm (Stm (..)) where
import Exp

data Stm = Assign String AExp
        | Skip
        | Concat [Stm]
        | If BExp Stm Stm
        | While BExp Stm
        deriving (Show)