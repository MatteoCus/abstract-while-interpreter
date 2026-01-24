module AbstractInterpreter (interpret) where
import qualified Data.Map.Lazy as Map
import CFG (Command (..), Label, Graph, arcsTo, Arc, findLoopLabels)
import Exp (AExp (..), Comparison (..))
import IntervalDomain (Interval (..), Infinitable (..), AbstractDomain (..))
import qualified Data.Maybe
import Data.Map (fromList, empty)


data SmashedBottom = SmashedBottom
   deriving (Eq, Show)

type State = Either SmashedBottom (Map.Map String Interval)

stateLub :: State -> State -> State
stateLub (Left SmashedBottom) y = y
stateLub x (Left SmashedBottom) = x
stateLub (Right state1) (Right state2) =
    Right $ Map.unionWith (\i1 i2 -> IntervalDomain.lub [i1, i2]) state1 state2

stateWidening :: State -> State -> State
stateWidening (Left SmashedBottom) y = y
stateWidening x (Left SmashedBottom) = x
stateWidening (Right state1) (Right state2) =
    Right $ Map.unionWith (∇) state1 state2

evalAExp :: AExp -> State -> Interval
evalAExp _ (Left _) = Empty
evalAExp (ConstantRange l u) _ = Interval l u
evalAExp (Var x) (Right state) = Data.Maybe.fromMaybe Empty (Map.lookup x state)
evalAExp (Neg a) state = neg (evalAExp a state)
evalAExp (Sum a1 a2) state = evalAExp a1 state IntervalDomain.+ evalAExp a2 state
evalAExp (Sub a1 a2) state = evalAExp a1 state IntervalDomain.- evalAExp a2 state
evalAExp (Mul a1 a2) state = evalAExp a1 state IntervalDomain.* evalAExp a2 state
evalAExp (Div a1 a2) state = evalAExp a1 state IntervalDomain./ evalAExp a2 state

evalBExp :: Comparison -> State -> Bool
evalBExp (Equal a1 a2) state = evalAExp a1 state == evalAExp a2 state
evalBExp (SmallerOrEqual a1 a2) state = evalAExp a1 state < evalAExp a2 state
evalBExp (GreaterOrEqual a1 a2) state = evalAExp a1 state >= evalAExp a2 state
evalBExp (Smaller a1 a2) state = evalAExp a1 state < evalAExp a2 state
evalBExp (Greater a1 a2) state = evalAExp a1 state > evalAExp a2 state
evalBExp (Different a1 a2) state = evalAExp a1 state /= evalAExp a2 state

interpretCommand :: Command -> State -> State
interpretCommand _ (Left SmashedBottom) = Left SmashedBottom

interpretCommand (CAssign var aExp) (Right state) = 
    case evalAExp aExp (Right state) of
        Empty -> Left SmashedBottom
        Interval low maxim -> Right $ Map.insert var (Interval low maxim) state

interpretCommand (CSmallerOrEqual (Sub (Var x) (ConstantRange c _)) (ConstantRange 0 0) ) (Right state) = 
    case evalAExp (Var x) (Right state) of
        Empty -> Left SmashedBottom
        Interval a b 
            | a > c -> Left SmashedBottom
            | otherwise -> interpretCommand (CAssign x (ConstantRange a (min b c))) (Right state)

interpretCommand (CSmallerOrEqual (Sub (Var x) (Var y)) (ConstantRange 0 0)) (Right state) =  
    case (exp1, exp2) of
        (Empty, _) -> Left SmashedBottom
        (_, Empty) -> Left SmashedBottom
        (Interval a b, Interval c d)
            | a > d -> Left SmashedBottom
            | otherwise -> 
                let updatedState = interpretCommand (CAssign x (ConstantRange a (min b d))) (Right state)
                in interpretCommand (CAssign y (ConstantRange (max c a) d)) updatedState
   where exp1 = evalAExp (Var x) (Right state)
         exp2 = evalAExp (Var y) (Right state)
interpretCommand _ state = state

buildStartingConfiguration :: [Label] -> Label -> Map.Map Label State
buildStartingConfiguration labels entryLabel = do
                                                   let entryState = (entryLabel, Right Map.empty)
                                                   fromList $ entryState : [(label, Left SmashedBottom) | label <- labels, label /= entryLabel]

interpret :: Graph -> Map.Map Label State
interpret ([], _, _, _) = Data.Map.empty
interpret graph@(labels, entry, _, _) = do
                  let startingConfiguration = buildStartingConfiguration labels entry
                  let wideningLabels = findLoopLabels graph [] []
                  interpret' graph wideningLabels startingConfiguration

interpret' :: Graph -> [Label] -> Map.Map Label State -> Map.Map Label State
interpret' graph@(labels, _, _, arcs) wideningLabels actualConfiguration = do
                     let newStates = map (\label -> updateState label arcs wideningLabels actualConfiguration) labels
                     let newConfiguration = Map.fromList $ zip labels newStates
                     if actualConfiguration == newConfiguration
                        then actualConfiguration
                        else interpret' graph wideningLabels newConfiguration


updateState :: Label -> [Arc] -> [Label] -> Map.Map Label State -> State
updateState actualLabel arcs wideningLabels actualConfiguration = do
                     let entryArcs = arcsTo actualLabel arcs
                     let associatedPreviousStates = map (\(en,_,_) -> case Map.lookup en actualConfiguration
                                                                      of Nothing -> Left SmashedBottom
                                                                         Just state -> state) entryArcs
                     let zippedStateCommands = zip (map (\(_,cm,_) -> cm) entryArcs) associatedPreviousStates
                     let calculatedStates = map (uncurry interpretCommand) zippedStateCommands
                     let newState = foldr stateLub (Right Map.empty) calculatedStates
                     let state = Map.lookup actualLabel actualConfiguration
                     case state
                        of Nothing -> newState
                           Just s ->
                              if actualLabel `elem` wideningLabels
                              then stateWidening s newState
                              else newState