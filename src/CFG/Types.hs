module CFG.Types (Command(..), Label, Arc, Graph, arcsTo, arcsFrom, freeVariablesCom) where
import Exp (AExp, Comparison (..), freeVariablesExp)
import Data.Set (Set)
import qualified Data.Set as Set

data Command = CAssign String AExp | CGuard Comparison
                deriving (Show)

type Label = Int

type Arc = (Label, Command, Label)

type Graph = (Set Label, Label, Label, [Arc])

arcsTo :: Label -> [Arc] -> [Arc]
arcsTo exitLabel = filter (\(_,_,exit) -> exit == exitLabel)

arcsFrom :: Label -> [Arc]  -> [Arc]
arcsFrom entryLabel = filter (\(entry,_,_) -> entry == entryLabel)

freeVariablesCom :: Command -> Set String
freeVariablesCom (CAssign x expr) = Set.singleton x `Set.union`freeVariablesExp expr
freeVariablesCom (CGuard (Equal exp1 exp2)) = freeVariablesExp exp1 `Set.union` freeVariablesExp exp2
freeVariablesCom (CGuard (Smaller exp1 exp2)) = freeVariablesExp exp1 `Set.union` freeVariablesExp exp2
freeVariablesCom (CGuard (SmallerOrEqual exp1 exp2)) = freeVariablesExp exp1 `Set.union` freeVariablesExp exp2
freeVariablesCom (CGuard (Greater exp1 exp2)) = freeVariablesExp exp1 `Set.union` freeVariablesExp exp2
freeVariablesCom (CGuard (GreaterOrEqual exp1 exp2)) = freeVariablesExp exp1 `Set.union` freeVariablesExp exp2
freeVariablesCom (CGuard (Different exp1 exp2)) = freeVariablesExp exp1 `Set.union` freeVariablesExp exp2