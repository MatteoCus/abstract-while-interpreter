module AbstractInterpreter (interpret) where
import qualified Data.Map.Lazy as Map
import CFG (findLoopLabels)
import Exp (AExp (..), Comparison (..))
import IntervalDomain (Interval (..), Infinitable(..), AbstractDomain (..))
import qualified Data.Maybe
import Data.Map (fromList)
import CFG.Types (Command(..), Graph, Label, Arc, arcsTo)
import AbstractInterpreter.State (State, SmashedBottom (..), stateWidening, stateNarrowing, stateLub)
import Data.Set (Set)
import qualified Data.Set as Set

evalAExp :: AExp -> State -> Interval
evalAExp _ (Left _) = Empty
evalAExp (ConstantRange l u) _ = Interval l u
evalAExp (Var x) (Right state) = Data.Maybe.fromMaybe Empty (Map.lookup x state)
evalAExp (Neg a) state = neg (evalAExp a state)
evalAExp (Sum a1 a2) state = evalAExp a1 state IntervalDomain.+ evalAExp a2 state
evalAExp (Sub a1 a2) state = evalAExp a1 state IntervalDomain.- evalAExp a2 state
evalAExp (Mul a1 a2) state = evalAExp a1 state IntervalDomain.* evalAExp a2 state
evalAExp (Div a1 a2) state = evalAExp a1 state IntervalDomain./ evalAExp a2 state

interpretCommand :: Command -> State -> State
interpretCommand _ (Left SmashedBottom) = Left SmashedBottom

interpretCommand (CAssign var aExp) (Right state) =
    case evalAExp aExp (Right state) of
        Empty -> Left SmashedBottom
        Interval low maxim -> Right $ Map.insert var (Interval low maxim) state

interpretCommand (CGuard (SmallerOrEqual expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state)
                                                                                                            let refinedValue = value `glb` Interval NegativeInfinity (Regular 0)
                                                                                                            propagateRefinedValue expr refinedValue (Right state)
interpretCommand (CGuard (Smaller expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state)
                                                                                                            let refinedValue = value `glb` Interval NegativeInfinity (Regular $ -1)
                                                                                                            propagateRefinedValue expr refinedValue (Right state)
interpretCommand (CGuard (GreaterOrEqual expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state)
                                                                                                            let refinedValue = value `glb` Interval (Regular 0) PositiveInfinity
                                                                                                            propagateRefinedValue expr refinedValue (Right state)
interpretCommand (CGuard (Greater expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do         
                                                                                                            let value = evalAExp expr (Right state)
                                                                                                            let refinedValue = value `glb` Interval (Regular 1) PositiveInfinity
                                                                                                            propagateRefinedValue expr refinedValue (Right state)
interpretCommand (CGuard (Equal expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state)
                                                                                                            let refinedValue = value `glb` Interval (Regular 0) (Regular 0)
                                                                                                            propagateRefinedValue expr refinedValue (Right state)

interpretCommand _ state = state

propagateRefinedValue :: AExp -> Interval -> State -> State
propagateRefinedValue _ _ (Left SmashedBottom) = Left SmashedBottom
propagateRefinedValue (Var x) value (Right state) = do
                                                    let lookOldValue = Map.lookup x state
                                                    case lookOldValue
                                                        of  Nothing -> Left SmashedBottom
                                                            (Just oldValue) -> case oldValue `glb` value
                                                                                of Empty -> Left SmashedBottom
                                                                                   Interval a b -> Right $ Map.insert x (Interval a b) state
propagateRefinedValue (ConstantRange a b) value state = case Interval a b `glb` value
                                                                    of  Empty -> Left SmashedBottom
                                                                        Interval _ _ -> state
propagateRefinedValue (Neg expr) value state = propagateRefinedValue expr (neg value) state
propagateRefinedValue (Sum exp1 exp2) value state = do
                                                        let state1 = propagateRefinedValue exp1 (value IntervalDomain.- evalAExp exp2 state) state
                                                        propagateRefinedValue exp2 (value IntervalDomain.- evalAExp exp1 state) state1
                                                        
propagateRefinedValue (Sub exp1 exp2) value state = do
                                                        let state1 = propagateRefinedValue exp1 (value IntervalDomain.+ evalAExp exp2 state) state
                                                        propagateRefinedValue exp2 (evalAExp exp1 state IntervalDomain.- value ) state1
                                                        
propagateRefinedValue (Mul exp1 exp2) value state = do
                                                        let state1 = propagateRefinedValue exp1 (value IntervalDomain./ evalAExp exp2 state) state
                                                        propagateRefinedValue exp2 (value IntervalDomain./ evalAExp exp1 state) state1
                                                        
propagateRefinedValue (Div exp1 exp2) value state = do
                                                        let state1 = propagateRefinedValue exp1 (value IntervalDomain.* evalAExp exp2 state) state
                                                        propagateRefinedValue exp2 (evalAExp exp1 state IntervalDomain./ value ) state1
                                                       

buildStartingConfiguration :: Set Label -> Label -> Map.Map Label State
buildStartingConfiguration labels entryLabel = do
    let entryState = (entryLabel, Right Map.empty)
    let otherLabels = [(label, Left SmashedBottom) | label <- Set.toList labels, label /= entryLabel]
    fromList $ entryState : otherLabels

interpret :: Graph -> Map.Map Label State
interpret graph@(labels, _, _, _)
    | Set.null labels = Map.empty
    | otherwise = do
        let (_, entry, _, _) = graph
        let startingConfiguration = buildStartingConfiguration labels entry
        let loopInvariantLabels = findLoopLabels graph Set.empty Set.empty
        narrowing graph loopInvariantLabels (interpret' graph loopInvariantLabels startingConfiguration)

interpret' :: Graph -> Set Label -> Map.Map Label State -> Map.Map Label State
interpret' graph@(labels, _, _, arcs) wideningLabels actualConfiguration = do
    let labelList = Set.toList labels
    let newStates = map (\label -> refineWith stateWidening label arcs wideningLabels actualConfiguration) labelList
    let newConfiguration = Map.fromList $ zip labelList newStates
    if actualConfiguration == newConfiguration
        then actualConfiguration
        else interpret' graph wideningLabels newConfiguration

narrowing :: Graph -> Set Label -> Map.Map Label State -> Map.Map Label State
narrowing graph@(labels, _, _, arcs) narrowingLabels actualConfiguration = do
    let labelList = Set.toList labels
    let newStates = map (\label -> refineWith stateNarrowing label arcs narrowingLabels actualConfiguration) labelList
    let newConfiguration = Map.fromList $ zip labelList newStates
    if actualConfiguration == newConfiguration
        then actualConfiguration
        else narrowing graph narrowingLabels newConfiguration

refineWith :: (State -> State -> State) -> Label -> [Arc] -> Set Label -> Map.Map Label State -> State
refineWith refinementAlg actualLabel arcs wideningLabels actualConfiguration = do
    let entryArcs = arcsTo actualLabel arcs
    let associatedPreviousStates = map (\(en,_,_) ->
            case Map.lookup en actualConfiguration of
                Nothing -> Left SmashedBottom
                Just state -> state) entryArcs
    let zippedStateCommands = zip (map (\(_,cm,_) -> cm) entryArcs) associatedPreviousStates
    let calculatedStates = map (uncurry interpretCommand) zippedStateCommands
    let newState = foldr stateLub (Right Map.empty) calculatedStates
    let state = Map.lookup actualLabel actualConfiguration
    case state of
        Nothing -> newState
        Just s ->
            if actualLabel `Set.member` wideningLabels
            then refinementAlg s newState
            else newState