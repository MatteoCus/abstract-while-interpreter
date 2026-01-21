module Exp (AExp (..), BExp (..)) where
    data AExp = Var String
                | AConstant Integer
                | Neg AExp
                | Sum AExp AExp
                | Sub AExp AExp
                | Mul AExp AExp
                | Div AExp AExp
            deriving (Show)

    data BExp =   Equal AExp AExp
                | SmallerOrEqual AExp AExp
            deriving (Show)