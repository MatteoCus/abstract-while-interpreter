module Exp (AExp (..), Comparison (..)) where
    data AExp = Var String
                | ConstantRange Integer Integer
                | Neg AExp
                | Sum AExp AExp
                | Sub AExp AExp
                | Mul AExp AExp
                | Div AExp AExp
            deriving (Show)

    data Comparison = Equal AExp AExp
                        | Smaller AExp AExp
                        | SmallerOrEqual AExp AExp
                        | Greater AExp AExp
                        | GreaterOrEqual AExp AExp
                        | Different AExp AExp
                        deriving (Show)