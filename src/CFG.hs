module CFG (buildCFG )where
import Exp (AExp, Comparison (..))
import Stm (Stm (..))
import Data.List (find)

data Command = CAssign String AExp
                | CEqual AExp AExp
                | CSmaller AExp AExp
                | CSmallerOrEqual AExp AExp
                | CGreater AExp AExp
                | CGreaterOrEqual AExp AExp
                | CDifferent AExp AExp
                deriving (Show)

type Label = Int

type Arc = (Label, Command, Label)

type Graph = ([Label], Label, Label, [Arc])

buildCFG :: [Stm] -> Graph -> Label -> Graph
buildCFG [] graph _ = graph
buildCFG (x : xs) graph label = buildCFG xs newGraph exit
                                where newGraph@(_, _, exit, _) = buildCFG' x graph label


-- Updates the graph adding labels and arcs related to the involved statement
buildCFG' :: Stm -> Graph -> Label -> Graph

buildCFG' Skip graph _ = graph

buildCFG' (Assign var expr) (labels, entry, exit, arcs) label = do
    let (newLabels, newEntry, newExit, newArcs) = updateLabelsSingle (labels, entry, exit, arcs)
    let newArc = (label, CAssign var expr, newExit)
    (newLabels, newEntry, newExit, newArc : newArcs)

buildCFG' (Concat stms) graph label = buildCFG stms graph label

buildCFG' (If comp stm1 stm2) (labels, entry, exit, arcs) label =
                                        case comp
                                        of  (Equal aExp1 aExp2) -> do
                                                                    let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                    let thenArc = (label, CEqual aExp1 aExp2, head (tail newLabels))
                                                                    let elseArc = (label, CDifferent aExp1 aExp2, head newLabels)
                                                                    let thenGraphStart = (newLabels, newEntry, head (tail newLabels), elseArc : thenArc : newArcs)
                                                                    let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG' stm1 thenGraphStart (head (tail newLabels))
                                                                    let twoExitsGraph = buildCFG' stm2 (thenLabels, thenEntry, head newLabels, thenArcs) (head newLabels)
                                                                    reconnectBranches twoExitsGraph thenExit
                                            (Different aExp1 aExp2) -> do
                                                                    let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                    let thenArc = (label, CDifferent aExp1 aExp2, head (tail newLabels))
                                                                    let elseArc = (label, CEqual aExp1 aExp2, head newLabels)
                                                                    let thenGraph = (newLabels, newEntry, head (tail newLabels), elseArc : thenArc : newArcs)
                                                                    let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG' stm1 thenGraph (head (tail newLabels))
                                                                    let twoExitsGraph = buildCFG' stm2 (thenLabels, thenEntry, head newLabels, thenArcs) (head newLabels)
                                                                    reconnectBranches twoExitsGraph thenExit
                                            (Greater aExp1 aExp2) -> do
                                                                    let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                    let thenArc = (label, CGreater aExp1 aExp2, head (tail newLabels))
                                                                    let elseArc = (label, CSmallerOrEqual aExp1 aExp2, head newLabels)
                                                                    let thenGraph = (newLabels, newEntry, head (tail newLabels), elseArc : thenArc : newArcs)
                                                                    let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG' stm1 thenGraph (head (tail newLabels))
                                                                    let twoExitsGraph = buildCFG' stm2 (thenLabels, thenEntry, head newLabels, thenArcs) (head newLabels)
                                                                    reconnectBranches twoExitsGraph thenExit
                                            (GreaterOrEqual aExp1 aExp2) -> do
                                                                    let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                    let thenArc = (label, CGreaterOrEqual aExp1 aExp2, head (tail newLabels))
                                                                    let elseArc = (label, CSmaller aExp1 aExp2, head newLabels)
                                                                    let thenGraph = (newLabels, newEntry, head (tail newLabels), elseArc : thenArc : newArcs)
                                                                    let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG' stm1 thenGraph (head (tail newLabels))
                                                                    let twoExitsGraph = buildCFG' stm2 (thenLabels, thenEntry, head newLabels, thenArcs) (head newLabels)
                                                                    reconnectBranches twoExitsGraph thenExit
                                            (Smaller aExp1 aExp2) -> do
                                                                    let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                    let thenArc = (label, CSmaller aExp1 aExp2, head (tail newLabels))
                                                                    let elseArc = (label, CGreaterOrEqual aExp1 aExp2, head newLabels)
                                                                    let thenGraph = (newLabels, newEntry, head (tail newLabels), elseArc : thenArc : newArcs)
                                                                    let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG' stm1 thenGraph (head (tail newLabels))
                                                                    let twoExitsGraph = buildCFG' stm2 (thenLabels, thenEntry, head newLabels, thenArcs) (head newLabels)
                                                                    reconnectBranches twoExitsGraph thenExit
                                            (SmallerOrEqual aExp1 aExp2) -> do
                                                                    let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                    let thenArc = (label, CSmallerOrEqual aExp1 aExp2, head (tail newLabels))
                                                                    let elseArc = (label, CGreater aExp1 aExp2, head newLabels)
                                                                    let thenGraph = (newLabels, newEntry, head (tail newLabels), elseArc : thenArc : newArcs)
                                                                    let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG' stm1 thenGraph (head (tail newLabels))
                                                                    let twoExitsGraph = buildCFG' stm2 (thenLabels, thenEntry, head newLabels, thenArcs) (head newLabels)
                                                                    reconnectBranches twoExitsGraph thenExit

buildCFG' _ (labels, entry, exit, arcs) _ = (labels, entry, exit, arcs)


-- Updates the graph adding a new label: the newly created label is also the new "exit" label
updateLabelsSingle :: Graph -> Graph
updateLabelsSingle (labels, entry, _, arcs) = do
    let maxLabel = foldr max 0 labels
    let newLabel = maxLabel + 1
    let newLabels = if null labels then [newLabel, maxLabel] else newLabel : labels
    (newLabels, entry, newLabel, arcs)

-- Updates the graph adding two new labels: the previous "latest" label is the new "exit" label
updateLabelsDouble :: Graph -> Label -> Graph
updateLabelsDouble (labels, entry, _, arcs) oldLabel = do
    let maxLabel = foldr max 0 labels
    let newLabel1 = maxLabel + 1
    let newLabel2 = maxLabel + 2
    let newLabels = if null labels then [newLabel2, newLabel1, oldLabel] else newLabel2 : newLabel1 : labels
    (newLabels, entry, oldLabel, arcs)

reconnectBranches :: Graph -> Label -> Graph
reconnectBranches (elseLabels, elseEntry, elseExit, elseArcs) thenExit = do
    let newLabels = filter (/= elseExit) elseLabels

    let foundArcsToUpdate = filter (\(_,_,exitLab) -> exitLab == elseExit) elseArcs

    let updatedArcs = arcUpdate foundArcsToUpdate thenExit

    let newArcs = filter (\(_,_,exitLab) -> exitLab /= elseExit) elseArcs

    (newLabels, elseEntry, thenExit, updatedArcs ++ newArcs)

arcUpdate :: [Arc] -> Label -> [Arc]
arcUpdate [] _ = []
arcUpdate (x : xs) newExit = (arcUpdate' x newExit ): arcUpdate xs newExit

arcUpdate' :: Arc -> Label -> Arc
arcUpdate' (oldElseEntryLab, oldCommand, _) newExit = (oldElseEntryLab, oldCommand, newExit)