module CFG.Graph (Command(..), Label, Arc, Graph) where
import Exp (AExp, Comparison (..))
import Data.Set (Set)

data Command = CAssign String AExp | CGuard Comparison
                deriving (Show, Eq, Ord)

type Label = Int

type Arc = (Label, Command, Label)

type Graph = (Set Label, Label, Label, Set Arc)