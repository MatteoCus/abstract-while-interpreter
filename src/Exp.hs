module Exp (AExp (..), Comparison (..), oppositeComparison) where

import IntervalDomain (Infinitable (..))
import Data.Set (singleton, Set)
import qualified Data.Set as Set

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

freeVariables :: AExp -> Set String
freeVariables (Var x) = singleton x
freeVariables (ConstantRange _ _) = Set.empty
freeVariables (Neg ex) = freeVariables ex
freeVariables (Sum exp1 exp2) = Set.union (freeVariables exp1) (freeVariables exp2)
freeVariables (Sub exp1 exp2) = Set.union (freeVariables exp1) (freeVariables exp2)
freeVariables (Mul exp1 exp2) = Set.union (freeVariables exp1) (freeVariables exp2)
freeVariables (Div exp1 exp2) = Set.union (freeVariables exp1) (freeVariables exp2)

oppositeComparison :: Comparison -> Comparison
oppositeComparison (Equal exp1 exp2) = Different exp1 exp2
oppositeComparison (Different exp1 exp2) = Equal exp1 exp2
oppositeComparison (Smaller exp1 exp2) = GreaterOrEqual exp1 exp2
oppositeComparison (GreaterOrEqual exp1 exp2) = Smaller exp1 exp2
oppositeComparison (Greater exp1 exp2) = SmallerOrEqual exp1 exp2
oppositeComparison (SmallerOrEqual exp1 exp2) = Greater exp1 exp2