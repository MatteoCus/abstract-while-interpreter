module CFG.Types (Command(..), Label, Arc, Graph, arcsTo, arcsFrom) where
import Exp (AExp, Comparison)
import Data.Set (Set)

data Command = CAssign String AExp | CGuard Comparison
                deriving (Show)

type Label = Int

type Arc = (Label, Command, Label)

type Graph = (Set Label, Label, Label, [Arc])

arcsTo :: Label -> [Arc] -> [Arc]
arcsTo exitLabel arcs = filter (\(_,_,exit) -> exit == exitLabel) arcs

arcsFrom :: Label -> [Arc]  -> [Arc]
arcsFrom entryLabel arcs = filter (\(entry,_,_) -> entry == entryLabel) arcs