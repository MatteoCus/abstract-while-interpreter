module AbstractInterpreter.StateOperations (stateLub, stateWidening, stateNarrowing) where
import IntervalDomain (AbstractDomain (lub, (∇), glb))
import qualified Data.Map.Lazy as Map
import RuntimeConfiguration (RuntimeConfig (descendingSteps))
import AbstractInterpreter.State (State, SmashedBottom (..))

stateLub :: State -> State -> RuntimeConfig -> State
stateLub (Left SmashedBottom) y _ = y
stateLub x (Left SmashedBottom) _ = x
stateLub (Right state1) (Right state2) config =
    Right $ Map.unionWith (\s1 s2 -> IntervalDomain.lub s1 s2 config) state1 state2

stateWidening :: State -> State -> State
stateWidening (Left SmashedBottom) y = y
stateWidening x (Left SmashedBottom) = x
stateWidening (Right state1) (Right state2) =
    Right $ Map.unionWith (∇) state1 state2

stateNarrowing :: RuntimeConfig -> State -> State -> State
stateNarrowing _ (Left SmashedBottom) y = y
stateNarrowing _ x (Left SmashedBottom) = x
stateNarrowing config (Right state1) (Right state2) 
    | descendingSteps config > 0 = Right $ Map.unionWith (\s1 s2 -> IntervalDomain.glb s1 s2 config) state1 state2
    | otherwise = Right state1

-- Best narrowing (not included because of assignment constraints)
-- stateNarrowing :: State -> State -> State
-- stateNarrowing (Left SmashedBottom) y = y
-- stateNarrowing x (Left SmashedBottom) = x
-- stateNarrowing (Right state1) (Right state2) =
--     Right $ Map.unionWith (△) state1 state2