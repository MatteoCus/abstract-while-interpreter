{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module ConcreteInterpreter (exec, State) where

import Exp (AExp (..), BExp (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Stm (Stm (..))

type State = Map.Map String Integer

evalAExp :: AExp -> State -> Integer
evalAExp (AConstant n) _ = n
evalAExp (Var x) state = case Map.lookup x state
                         of Nothing -> error ("La variabile non esiste!")
                            Just v -> v
evalAExp (Neg a) state = - evalAExp a state
evalAExp (Sum a1 a2) state = (evalAExp a1 state) + (evalAExp a2 state)
evalAExp (Sub a1 a2) state = (evalAExp a1 state) - (evalAExp a2 state)
evalAExp (Mul a1 a2) state = (evalAExp a1 state) * (evalAExp a2 state)
evalAExp (Div a1 a2) state = (evalAExp a1 state) `div` (evalAExp a2 state)

evalBExp :: BExp -> State -> Bool
evalBExp (Equal a1 a2) state = (evalAExp a1 state) == (evalAExp a2 state)
evalBExp (SmallerOrEqual a1 a2) state = (evalAExp a1 state) < (evalAExp a2 state)

exec :: Stm -> State -> State
exec (Skip) state = state
exec (Assign var val) state = Map.insert var (evalAExp val state) state
exec (Concat []) state = state
exec (Concat (x : xs)) state = exec (Concat xs) (exec x state)
exec (If b stm1 stm2) state = if evalBExp b state then exec stm1 state else exec stm2 state
exec (While b stm) state = if evalBExp b state then exec (While b stm) (exec stm state) else state