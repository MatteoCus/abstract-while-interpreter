module CFG (buildCFG, findLoopLabels, normalize) where
import Exp (oppositeComparison)
import Stm (Stm (..))
import CFG.Types (Label, Graph, Command(..), Arc)
import Data.Set (Set)
import qualified Data.Set as Set


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
    let [elseLabel, thenLabel] = take 2 $ Set.toDescList newLabels  -- Get two largest labels
    let thenArc = (label, CGuard comparison, thenLabel)
    let elseArc = (label, CGuard (oppositeComparison comparison), elseLabel)
    let thenGraphStart = (newLabels, newEntry, thenLabel, elseArc : thenArc : newArcs)
    let (thenLabels, thenEntry, thenExit, thenArcs) = buildCFG stm1 thenGraphStart thenLabel
    let twoExitsGraph = buildCFG stm2 (thenLabels, thenEntry, elseLabel, thenArcs) elseLabel
    reconnectBranches twoExitsGraph thenExit

buildCFG (While comparison stm) (labels, entry, exit, arcs) label = do
    let (newLabels, newEntry, _, newArcs) = updateLabelsDouble (labels, entry, exit, arcs) label
    let [exitLabel, bodyLabel] = take 2 $ Set.toDescList newLabels  -- Get two largest labels
    let bodyArc = (label, CGuard comparison, bodyLabel)
    let exitArc = (label, CGuard (oppositeComparison comparison), exitLabel)
    let bodyGraphStart = (newLabels, newEntry, bodyLabel, exitArc : bodyArc : newArcs)
    let openWhileGraph = buildCFG stm bodyGraphStart bodyLabel
    let (whileLabels, whileEntry, _, whileArcs) = reconnectBranches openWhileGraph exit
    (whileLabels, whileEntry, exitLabel, whileArcs)


-- Updates the graph adding a new label: the newly created label is also the new "exit" label
updateLabelsSingle :: Graph -> Graph
updateLabelsSingle (labels, entry, _, arcs) = do
    let maxLabel = if Set.null labels then 0 else Set.findMax labels
    let newLabel = maxLabel + 1
    let newLabels = if Set.null labels 
                    then Set.fromList [newLabel, maxLabel] 
                    else Set.insert newLabel labels
    (newLabels, entry, newLabel, arcs)

-- Updates the graph adding two new labels: the previous "latest" label is the new "exit" label
updateLabelsDouble :: Graph -> Label -> Graph
updateLabelsDouble (labels, entry, _, arcs) oldLabel = do
    let maxLabel = if Set.null labels then 0 else Set.findMax labels
    let newLabel1 = maxLabel + 1
    let newLabel2 = maxLabel + 2
    let newLabels = if Set.null labels 
                    then Set.fromList [newLabel2, newLabel1, oldLabel] 
                    else Set.insert newLabel2 (Set.insert newLabel1 labels)
    (newLabels, entry, oldLabel, arcs)

reconnectBranches :: Graph -> Label -> Graph
reconnectBranches (elseLabels, elseEntry, elseExit, elseArcs) thenExit = do
    let newLabels = Set.delete elseExit elseLabels
    let foundArcsToUpdate = filter (\(_,_,exitLab) -> exitLab == elseExit) elseArcs
    let updatedArcs = arcUpdate foundArcsToUpdate thenExit
    let newArcs = filter (\(_,_,exitLab) -> exitLab /= elseExit) elseArcs
    (newLabels, elseEntry, thenExit, updatedArcs ++ newArcs)

arcUpdate :: [Arc] -> Label -> [Arc]
arcUpdate [] _ = []
arcUpdate (x : xs) newExit = arcUpdate' x newExit : arcUpdate xs newExit

arcUpdate' :: Arc -> Label -> Arc
arcUpdate' (oldElseEntryLab, oldCommand, _) newExit = (oldElseEntryLab, oldCommand, newExit)

findLoopLabels :: Graph -> Set Label -> Set Label -> Set Label
findLoopLabels (_, _, _, []) _ _ = Set.empty
findLoopLabels (labels, en, ex, arcs) foundLabels alreadyExamined
    | Set.null labels = Set.empty
    | en `Set.member` foundLabels = foundLabels
    | en `Set.member` alreadyExamined = foundLabels
    | labels == alreadyExamined = foundLabels
    | otherwise = 
        let nextLabels = Set.fromList $ map (\(_,_,t) -> t) $ filter (\(entry,_,_) -> entry == en) arcs
            hasLoop = any (findLoopLabels' arcs alreadyExamined en) (Set.toList nextLabels)
            newFound = if hasLoop then Set.insert en foundLabels else foundLabels
            newExamined = Set.insert en alreadyExamined
        in Set.unions $ map (\x -> findLoopLabels (labels, x, ex, arcs) newFound newExamined) (Set.toList nextLabels)


findLoopLabels' :: [Arc] -> Set Label -> Label -> Label -> Bool
findLoopLabels' [] _ _ _ = False
findLoopLabels' arcs alreadyExploredLabels toFindLabel actualLabel
    | actualLabel `Set.member` alreadyExploredLabels = False
    | actualLabel == toFindLabel = True
    | otherwise = 
        let nextArcs = filter (\(en,_, _) -> en == actualLabel) arcs
            newExplored = Set.insert actualLabel alreadyExploredLabels
        in any (findLoopLabels' arcs newExplored toFindLabel . (\(_,_,t) -> t)) nextArcs

normalize :: Graph -> Graph 
normalize graph@(labels,_,_,_) = do
    let maxim =  Set.lookupMax labels
    case maxim
        of  Nothing -> graph
            (Just mx) -> normalize' [0..mx] graph

normalize' :: [Int] -> Graph -> Graph 
normalize' [] graph = graph
normalize' (x : xs) graph@(labels, _, _, _) = if x `Set.member` labels
                                                 then normalize' xs graph
                                                 else normalize' xs (updateMissingLabel x graph)

updateMissingLabel :: Int -> Graph -> Graph
updateMissingLabel missingLabel (labels, entry,exit, arcs) = do
    let newLabels = Set.map (\label -> if label > missingLabel then label-1 else label ) labels
    let newArcs = Prelude.map (\(en,c,ex) -> case (en > missingLabel, ex > missingLabel)
                                             of (True, True) -> (en-1,c,ex-1) 
                                                (True, False) -> (en-1,c,ex)
                                                (False, True) -> (en,c,ex-1)
                                                (False, False) -> (en,c,ex)) arcs
    let newExit = if exit > missingLabel then exit-1 else exit
    (newLabels, entry, newExit, newArcs)