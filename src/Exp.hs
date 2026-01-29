module Exp (AExp (..), Comparison (..), oppositeComparison) where

import IntervalDomain (Infinitable (..))

data AExp = Var String
                | ConstantRange (Infinitable Integer) (Infinitable Integer)
                | Neg AExp
                | Sum AExp AExp
                | Sub AExp AExp
                | Mul AExp AExp
                | Div AExp AExp
            deriving (Show)

data Comparison =   Equal AExp AExp
                    | Smaller AExp AExp
                    | SmallerOrEqual AExp AExp
                    | Greater AExp AExp
                    | GreaterOrEqual AExp AExp
                    | Different AExp AExp
                deriving (Show)

oppositeComparison :: Comparison -> Comparison
oppositeComparison (Equal exp1 exp2) = Different exp1 exp2
oppositeComparison (Different exp1 exp2) = Equal exp1 exp2
oppositeComparison (Smaller exp1 exp2) = GreaterOrEqual exp1 exp2
oppositeComparison (GreaterOrEqual exp1 exp2) = Smaller exp1 exp2
oppositeComparison (Greater exp1 exp2) = SmallerOrEqual exp1 exp2
oppositeComparison (SmallerOrEqual exp1 exp2) = Greater exp1 exp2