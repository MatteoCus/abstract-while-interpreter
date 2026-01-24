module CFG (buildCFG, findLoopLabels, Command (..))where
import Exp (AExp, Comparison (..))
import Stm (Stm (..))
import Data.List (find, sort)

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


-- Updates the graph adding labels and arcs related to the involved statement
buildCFG :: Stm -> Graph -> Label -> Graph

buildCFG Skip graph _ = graph

buildCFG (Assign var expr) (labels, entry, exit, arcs) label = do
    let (newLabels, newEntry, newExit, newArcs) = updateLabelsSingle (labels, entry, exit, arcs)
    let newArc = (label, CAssign var expr, newExit)
    (newLabels, newEntry, newExit, newArc : newArcs)

buildCFG (Concat []) graph _ = graph

buildCFG (Concat (x : xs)) graph label = buildCFG (Concat xs) newGraph exit
                                where newGraph@(_, _, exit, _) = buildCFG x graph label

buildCFG (If comparison stm1 stm2) (labels, entry, exit, arcs) label = do
                                                                        let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                        let thenLabel = head $ tail newLabels
                                                                        let elseLabel = head newLabels
                                                                        let thenArc = (label, translateComparisonCommand comparison, thenLabel)
                                                                        let elseArc = (label, translateComparisonCommand $ oppositeComparison comparison, elseLabel)
                                                                        let thenGraphStart = (newLabels, newEntry, thenLabel, elseArc : thenArc : newArcs)
                                                                        let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG stm1 thenGraphStart thenLabel
                                                                        let twoExitsGraph = buildCFG stm2 (thenLabels, thenEntry, elseLabel, thenArcs) elseLabel
                                                                        reconnectBranches twoExitsGraph thenExit

buildCFG (While comparison stm) (labels, entry, exit, arcs) label = do
                                                                        let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
                                                                        let bodyLabel = head $ tail newLabels
                                                                        let exitLabel = head newLabels
                                                                        let bodyArc = (label, translateComparisonCommand comparison, bodyLabel)
                                                                        let exitArc = (label, translateComparisonCommand $ oppositeComparison comparison, exitLabel)
                                                                        let bodyGraphStart = (newLabels, newEntry, bodyLabel, exitArc : bodyArc : newArcs)
                                                                        let openWhileGraph = buildCFG stm bodyGraphStart bodyLabel
                                                                        let (whileLabels, whileEntry, _, whileArcs) = reconnectBranches openWhileGraph exit
                                                                        (whileLabels, whileEntry, exitLabel, whileArcs)

oppositeComparison :: Comparison -> Comparison
oppositeComparison (Equal exp1 exp2) = Different exp1 exp2
oppositeComparison (Different exp1 exp2) = Equal exp1 exp2
oppositeComparison (Smaller exp1 exp2) = GreaterOrEqual exp1 exp2
oppositeComparison (GreaterOrEqual exp1 exp2) = Smaller exp1 exp2
oppositeComparison (Greater exp1 exp2) = SmallerOrEqual exp1 exp2
oppositeComparison (SmallerOrEqual exp1 exp2) = Greater exp1 exp2

translateComparisonCommand :: Comparison -> Command
translateComparisonCommand (Equal exp1 exp2) = CEqual exp1 exp2
translateComparisonCommand (Different exp1 exp2) = CDifferent exp1 exp2
translateComparisonCommand (Smaller exp1 exp2) = CSmaller exp1 exp2
translateComparisonCommand (GreaterOrEqual exp1 exp2) = CGreaterOrEqual exp1 exp2
translateComparisonCommand (Greater exp1 exp2) = CGreater exp1 exp2
translateComparisonCommand (SmallerOrEqual exp1 exp2) = CSmallerOrEqual exp1 exp2

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
arcUpdate (x : xs) newExit = (arcUpdate' x newExit): arcUpdate xs newExit

arcUpdate' :: Arc -> Label -> Arc
arcUpdate' (oldElseEntryLab, oldCommand, _) newExit = (oldElseEntryLab, oldCommand, newExit)

findLoopLabels :: Graph -> [Label] -> [Label] -> [Label]
findLoopLabels (_, _, _, []) _ _= []
findLoopLabels ([], _, _, _) _ _= []
findLoopLabels (labels, en, ex, arcs) foundLabels alreadyExamined
    | en `elem` foundLabels = foundLabels
    | en `elem` alreadyExamined = foundLabels
    | (sort labels) == (sort alreadyExamined) = foundLabels
    | otherwise = if any (findLoopLabels' arcs alreadyExamined en) nextLabels
                                                        then concatMap (\x -> findLoopLabels (labels, x, ex, arcs) (en:foundLabels) (en:alreadyExamined)) nextLabels
                                                        else concatMap (\x -> findLoopLabels (labels, x, ex, arcs) foundLabels (en:alreadyExamined)) nextLabels
                                                        where nextLabels = map (\(_,_,t) -> t) (filter (\(entry,_,_) -> entry == en) arcs)


findLoopLabels' :: [Arc] -> [Label] -> Label -> Label -> Bool
findLoopLabels' [] _ _ _= False
findLoopLabels' arcs alreadyExploredLabels toFindLabel actualLabel
    | actualLabel `elem` alreadyExploredLabels = False -- already explored node, not found
    | actualLabel == toFindLabel = True                                           -- found node
    | otherwise = any (findLoopLabels' arcs (actualLabel : alreadyExploredLabels) toFindLabel . (\(_,_,t) -> t)) (filter (\(en,_, _) -> en == actualLabel) arcs)

arcsTo :: [Arc] -> Label -> [Arc]
arcsTo arcs exitLabel = filter (\(_,_,exit) -> exit == exitLabel) arcs

arcsFrom :: [Arc] -> Label -> [Arc]
arcsFrom arcs entryLabel = filter (\(entry,_,_) -> entry == entryLabel) arcs