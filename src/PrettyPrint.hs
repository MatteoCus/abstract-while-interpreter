module PrettyPrint (prettyPrintStm, prettyPrintCFG, prettyPrintGraph, prettyPrintStates, prettyState, prettyInterval, prettyInfinitable, prettyRange) where

import Stm (Stm (..))
import Exp (AExp (..), Comparison (..))
import CFG.Graph (Command (..), Label, Arc, Graph)
import IntervalDomain (Interval (..), Infinitable (..))
import AbstractInterpreter.State (State, SmashedBottom (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, sortOn)

-- ============================================================================
-- AST Pretty Printing
-- ============================================================================

-- | Pretty print a statement (AST)
prettyPrintStm :: Stm -> String
prettyPrintStm = prettyStm 0

prettyStm :: Int -> Stm -> String
prettyStm indent Skip = indentStr indent ++ "skip"

prettyStm indent (Assign var expr) =
    indentStr indent ++ var ++ " := " ++ prettyAExp expr

prettyStm indent (Concat stms) =
    intercalate ";\n" (map (prettyStm indent) stms)

prettyStm indent (If cmp thenStm elseStm) =
    indentStr indent ++ "if " ++ prettyComparison cmp ++ " then\n" ++
    prettyStm (indent + 2) thenStm ++ "\n" ++
    indentStr indent ++ "else\n" ++
    prettyStm (indent + 2) elseStm ++ "\n" ++
    indentStr indent ++ "fi"

prettyStm indent (While cmp body) =
    indentStr indent ++ "while " ++ prettyComparison cmp ++ " do\n" ++
    prettyStm (indent + 2) body ++ "\n" ++
    indentStr indent ++ "done"

-- | Pretty print an arithmetic expression
prettyAExp :: AExp -> String
prettyAExp (Var x) = x
prettyAExp (ConstantRange l u) = prettyRange l u
prettyAExp (Neg a) = "-(" ++ prettyAExp a ++ ")"
prettyAExp (Sum a1 a2) = "(" ++ prettyAExp a1 ++ " + " ++ prettyAExp a2 ++ ")"
prettyAExp (Sub a1 a2) = "(" ++ prettyAExp a1 ++ " - " ++ prettyAExp a2 ++ ")"
prettyAExp (Mul a1 a2) = "(" ++ prettyAExp a1 ++ " * " ++ prettyAExp a2 ++ ")"
prettyAExp (Div a1 a2) = "(" ++ prettyAExp a1 ++ " / " ++ prettyAExp a2 ++ ")"

-- | Pretty print a comparison
prettyComparison :: Comparison -> String
prettyComparison (Equal a1 a2) = prettyAExp a1 ++ " = " ++ prettyAExp a2
prettyComparison (Smaller a1 a2) = prettyAExp a1 ++ " < " ++ prettyAExp a2
prettyComparison (SmallerOrEqual a1 a2) = prettyAExp a1 ++ " <= " ++ prettyAExp a2
prettyComparison (Greater a1 a2) = prettyAExp a1 ++ " > " ++ prettyAExp a2
prettyComparison (GreaterOrEqual a1 a2) = prettyAExp a1 ++ " >= " ++ prettyAExp a2
prettyComparison (Different a1 a2) = prettyAExp a1 ++ " <> " ++ prettyAExp a2

-- ============================================================================
-- CFG Pretty Printing
-- ============================================================================

-- | Pretty print a command
prettyCommand :: Command -> String
prettyCommand (CAssign var expr) = var ++ " := " ++ prettyAExp expr
prettyCommand (CGuard cmp) = prettyComparison cmp

-- | Pretty print a single arc
prettyArc :: Arc -> String
prettyArc (from, cmd, to) =
    show from ++ " --[ " ++ prettyCommand cmd ++ " ]--> " ++ show to

-- | Pretty print the entire CFG as a list of arcs
prettyPrintCFG :: Graph -> String
prettyPrintCFG (labels, entry, exit, arcs) =
    "CFG:\n" ++
    "  Entry: " ++ show entry ++ "\n" ++
    "  Exit:  " ++ show exit ++ "\n" ++
    "  Labels: " ++ show (Set.toList labels) ++ "\n" ++
    "  Arcs:\n" ++
    unlines (map (("    " ++) . prettyArc) (sortOn (\(f,_,_) -> f) arcs))

-- | Pretty print CFG as a graph visualization (DOT format)
prettyPrintGraph :: Graph -> String
prettyPrintGraph (_, entry, exit, arcs) =
    "digraph CFG {\n" ++
    "  rankdir=TB;\n" ++
    "  node [shape=circle];\n" ++
    "  " ++ show entry ++ " [shape=doublecircle, label=\"Entry\\n" ++ show entry ++ "\"];\n" ++
    "  " ++ show exit ++ " [shape=doublecircle, label=\"Exit\\n" ++ show exit ++ "\"];\n" ++
    concatMap prettyDotArc arcs ++
    "}\n"
  where
    prettyDotArc (from, cmd, to) =
        "  " ++ show from ++ " -> " ++ show to ++
        " [label=\"" ++ escapeDot (prettyCommand cmd) ++ "\"];\n"

    escapeDot = concatMap escape
      where
        escape '"' = "\\\""
        escape '\\' = "\\\\"
        escape c = [c]

-- ============================================================================
-- Analysis Results Pretty Printing
-- ============================================================================

-- | Pretty print an interval
prettyInterval :: Interval -> String
prettyInterval Empty = "⊥"
prettyInterval (Interval l@NegativeInfinity u@(Regular _)) = "(" ++ prettyInfinitable l ++ ", " ++ prettyInfinitable u ++ "]"
prettyInterval (Interval l@(Regular _) u@PositiveInfinity) = "[" ++ prettyInfinitable l ++ ", " ++ prettyInfinitable u ++ ")"
prettyInterval (Interval NegativeInfinity PositiveInfinity) = "⊤"
prettyInterval (Interval l u) = "[" ++ prettyInfinitable l ++ ", " ++ prettyInfinitable u ++ "]"

-- | Pretty print an infinitable value
prettyInfinitable :: Infinitable Integer -> String
prettyInfinitable NegativeInfinity = "-∞"
prettyInfinitable PositiveInfinity = "+∞"
prettyInfinitable (Regular n) = show n

-- | Pretty print a range (for constants)
prettyRange :: Infinitable Integer -> Infinitable Integer -> String
prettyRange l u
    | l == u = prettyInfinitable l
    | otherwise = "[" ++ prettyInfinitable l ++ ", " ++ prettyInfinitable u ++ "]"

-- | Pretty print a state
prettyState :: State -> String
prettyState (Left SmashedBottom) = "⊥"
prettyState (Right varMap)
    | Map.null varMap = "⊤"
    | otherwise = "{ " ++ intercalate ", " bindings ++ " }"
  where
    bindings = map (\(var, interval) -> var ++ " ↦ " ++ prettyInterval interval)
                   (Map.toList varMap)

-- | Pretty print all states at all labels
prettyPrintStates :: Map.Map Label State -> String
prettyPrintStates stateMap =
    "Analysis Results:\n" ++
    unlines (map prettyLabelState (sortOn fst $ Map.toList stateMap))
  where
    prettyLabelState (label, state) =
        "  Label " ++ show label ++ ": " ++ prettyState state

-- ============================================================================
-- Utilities
-- ============================================================================

indentStr :: Int -> String
indentStr n = replicate n ' '