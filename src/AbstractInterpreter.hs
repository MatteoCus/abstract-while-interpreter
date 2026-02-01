module AbstractInterpreter (interpret) where
import qualified Data.Map.Lazy as Map
import CFG (findLoopLabels)
import Exp (AExp (..), Comparison (..))
import IntervalDomain (Interval (..), AbstractDomain (..))
import Infinitable (Infinitable(..))
import qualified Data.Maybe
import Data.Map (fromList, fromSet)
import CFG.Graph (Command(..), Graph, Label, Arc)
import AbstractInterpreter.StateOperations (stateWidening, stateNarrowing, stateLub)
import Data.Set (Set)
import qualified Data.Set as Set
import RuntimeConfiguration (RuntimeConfig (..))
import AbstractInterpreter.State ( State, SmashedBottom(..) )
import CFG.GraphOperations (freeVariablesCom, arcsTo)

evalAExp :: AExp -> State -> RuntimeConfig -> Interval
evalAExp _ (Left _) _ = Empty
evalAExp (ConstantRange l u) _ config = do
                                            let (m,n) = intervalBounds config
                                            if m > n && l /= u
                                            then Interval NegativeInfinity PositiveInfinity
                                            else if l == u
                                                 then Interval l u
                                                 else
                                                    case (l < m, u > n, l > n, u < m)
                                                    of  (_,_,True,_) -> Interval NegativeInfinity PositiveInfinity
                                                        (_,_,_, True) -> Interval NegativeInfinity PositiveInfinity
                                                        (True, True, _, _) -> Interval NegativeInfinity PositiveInfinity
                                                        (True, False, _, _) -> Interval NegativeInfinity u
                                                        (False, True, _, _) -> Interval l PositiveInfinity
                                                        (False, False, _, _) -> Interval l u
evalAExp (Var x) (Right state) _ = Data.Maybe.fromMaybe Empty (Map.lookup x state)
evalAExp (Neg a) state config = neg (evalAExp a state config)
evalAExp (Sum a1 a2) state config = evalAExp a1 state config IntervalDomain.+ evalAExp a2 state config
evalAExp (Sub a1 a2) state config = evalAExp a1 state config IntervalDomain.- evalAExp a2 state config
evalAExp (Mul a1 a2) state config = evalAExp a1 state config IntervalDomain.* evalAExp a2 state config
evalAExp (Div a1 a2) state config = (IntervalDomain./) (evalAExp a1 state config) (evalAExp a2 state config) config

interpretCommand :: RuntimeConfig -> Command -> State  -> State
interpretCommand _ _ (Left SmashedBottom)= Left SmashedBottom

interpretCommand config (CAssign var aExp) (Right state) =
    case evalAExp aExp (Right state) config of
        Empty -> Left SmashedBottom
        Interval low maxim -> Right $ Map.insert var (Interval low maxim) state

interpretCommand config (CGuard (SmallerOrEqual expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state) config
                                                                                                            let refinedValue = glb value (Interval NegativeInfinity (Regular 0)) config
                                                                                                            propagateRefinedValue expr refinedValue (Right state) config
interpretCommand config (CGuard (Smaller expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state) config
                                                                                                            let refinedValue = glb value (Interval NegativeInfinity (Regular $ -1)) config
                                                                                                            propagateRefinedValue expr refinedValue (Right state) config
interpretCommand config (CGuard (GreaterOrEqual expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state) config
                                                                                                            let refinedValue = glb value (Interval (Regular 0) PositiveInfinity) config
                                                                                                            propagateRefinedValue expr refinedValue (Right state) config
interpretCommand config (CGuard (Greater expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state) config
                                                                                                            let refinedValue = glb value (Interval (Regular 1) PositiveInfinity) config
                                                                                                            propagateRefinedValue expr refinedValue (Right state) config
interpretCommand config (CGuard (Equal expr (ConstantRange (Regular 0) (Regular 0)))) (Right state) = do
                                                                                                            let value = evalAExp expr (Right state) config
                                                                                                            let refinedValue = glb value (Interval (Regular 0) (Regular 0)) config
                                                                                                            propagateRefinedValue expr refinedValue (Right state) config

interpretCommand _ _ state = state

propagateRefinedValue :: AExp -> Interval -> State -> RuntimeConfig -> State
propagateRefinedValue _ _ (Left SmashedBottom) _ = Left SmashedBottom
propagateRefinedValue _ Empty _ _ = Left SmashedBottom
propagateRefinedValue (Var x) value (Right state) config = do
                                                    let lookOldValue = Map.lookup x state
                                                    case lookOldValue
                                                        of  Nothing -> Left SmashedBottom
                                                            (Just oldValue) -> case glb oldValue value config
                                                                                of Empty -> Left SmashedBottom
                                                                                   Interval a b -> Right $ Map.insert x (Interval a b) state
propagateRefinedValue (ConstantRange a b) value state config = case glb (Interval a b) value config
                                                                    of  Empty -> Left SmashedBottom
                                                                        Interval _ _ -> state
propagateRefinedValue (Neg expr) value state config = propagateRefinedValue expr (neg value) state config
propagateRefinedValue (Sum exp1 exp2) value state config = do
                                                        let state1 = propagateRefinedValue exp1 (value IntervalDomain.- evalAExp exp2 state config) state config
                                                        propagateRefinedValue exp2 (value IntervalDomain.- evalAExp exp1 state config) state1 config

propagateRefinedValue (Sub exp1 exp2) value state config = do
                                                        let state1 = propagateRefinedValue exp1 (value IntervalDomain.+ evalAExp exp2 state config ) state config
                                                        propagateRefinedValue exp2 (evalAExp exp1 state config IntervalDomain.- value ) state1 config

propagateRefinedValue (Mul exp1 exp2) value state config = do
                                                        let state1 = propagateRefinedValue exp1 ((IntervalDomain./) value (evalAExp exp2 state config) config ) state config
                                                        propagateRefinedValue exp2 ((IntervalDomain./) value (evalAExp exp1 state config) config) state1 config

propagateRefinedValue (Div exp1 exp2) value state config = do
                                                        let state1 = propagateRefinedValue exp1 (value IntervalDomain.* evalAExp exp2 state config) state config
                                                        propagateRefinedValue exp2 ((IntervalDomain./) (evalAExp exp1 state config) value config ) state1 config


buildStartingConfiguration :: Set Label -> Label -> State -> Map.Map Label State
buildStartingConfiguration labels entryLabel entryState = do
    let entryConfiguration = (entryLabel, entryState)
    let otherLabels = [(label, Left SmashedBottom) | label <- Set.toList labels, label /= entryLabel]
    fromList $ entryConfiguration : otherLabels

interpret :: Graph -> RuntimeConfig -> Map.Map Label State
interpret graph@(labels, _, _, _) runtimeConfig
    | Set.null labels = Map.empty
    | otherwise = do
        let (_, entry, _, arcs) = graph
        let entryConfiguration = case (startingConfiguration runtimeConfig, buildEntryDefaultConfiguration arcs)
                                 of (Left SmashedBottom, defaultConfig) -> defaultConfig
                                    (Right state, Left SmashedBottom) -> Right state
                                    (Right state, Right defaultState) -> Right (Map.union state defaultState)
        let startConfiguration = buildStartingConfiguration labels entry entryConfiguration
        let loopInvariantLabels = findLoopLabels graph Set.empty Set.empty
        if enableNarrowing runtimeConfig
            then narrowing graph loopInvariantLabels (interpret' graph loopInvariantLabels startConfiguration runtimeConfig) runtimeConfig
            else interpret' graph loopInvariantLabels startConfiguration runtimeConfig

buildEntryDefaultConfiguration :: [Arc] -> State
buildEntryDefaultConfiguration arcs = Right $ fromSet (\_ -> Interval NegativeInfinity PositiveInfinity) (foldr (Set.union . (\(_,cm,_) -> freeVariablesCom cm)) Set.empty arcs)

interpret' :: Graph -> Set Label -> Map.Map Label State -> RuntimeConfig -> Map.Map Label State
interpret' graph@(labels, entry, _, arcs) wideningLabels actualConfiguration runtimeConfig = do
    let labelList = Set.toList labels
    let newStates = map (\label -> refineWith stateWidening label entry arcs wideningLabels actualConfiguration (enableWidening runtimeConfig) runtimeConfig) labelList
    let newConfiguration = Map.fromList $ zip labelList newStates
    if actualConfiguration == newConfiguration
        then actualConfiguration
        else interpret' graph wideningLabels newConfiguration runtimeConfig

narrowing :: Graph -> Set Label -> Map.Map Label State -> RuntimeConfig -> Map.Map Label State
narrowing graph@(labels, entry, _, arcs) narrowingLabels actualConfiguration runtimeConfig = do
    let labelList = Set.toList labels
    let newStates = map (\label -> refineWith stateNarrowing label entry arcs narrowingLabels actualConfiguration (enableNarrowing runtimeConfig) runtimeConfig) labelList
    let newConfiguration = Map.fromList $ zip labelList newStates
    if actualConfiguration == newConfiguration
        then actualConfiguration
        else narrowing graph narrowingLabels newConfiguration runtimeConfig

refineWith :: (State -> State -> State) -> Label -> Label -> [Arc] -> Set Label -> Map.Map Label State -> Bool -> RuntimeConfig -> State
refineWith refinementAlg actualLabel entryLabel arcs wideningLabels actualConfiguration shoulAlgBeExecuted runtimeConfig = do
    let entryArcs = arcsTo actualLabel arcs
    let associatedPreviousStates = map (\(en,_,_) ->
            case Map.lookup en actualConfiguration of
                Nothing -> Left SmashedBottom
                Just state -> state) entryArcs
    let zippedStateCommands = zip (map (\(_,cm,_) -> cm) entryArcs) associatedPreviousStates
    let calculatedStates_ = map (uncurry (interpretCommand runtimeConfig)) zippedStateCommands
    let currentState = Map.lookup actualLabel actualConfiguration
    let calculatedStates = if actualLabel == entryLabel && not (null calculatedStates_)
                      then case currentState of
                             Nothing -> calculatedStates_
                             Just s -> s : calculatedStates_
                      else calculatedStates_
    let newState = if null calculatedStates
                   then Data.Maybe.fromMaybe (Left SmashedBottom) currentState
                   else foldr1 (\s1 s2 -> stateLub s1 s2 runtimeConfig) calculatedStates
    case currentState
        of  Nothing -> newState
            Just s ->
                    if actualLabel `Set.member` wideningLabels && shoulAlgBeExecuted
                    then refinementAlg s newState
                    else newState