module AbstractInterpreter.State (State, SmashedBottom(..), stateLub, stateWidening, stateNarrowing) where
import IntervalDomain (Interval, AbstractDomain (lub, (∇), (△)))
import qualified Data.Map.Lazy as Map
import RuntimeConfiguration (RuntimeConfig)

data SmashedBottom = SmashedBottom
   deriving (Eq, Show)

type State = Either SmashedBottom (Map.Map String Interval)

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

stateNarrowing :: State -> State -> State
stateNarrowing (Left SmashedBottom) y = y
stateNarrowing x (Left SmashedBottom) = x
stateNarrowing (Right state1) (Right state2) =
    Right $ Map.unionWith (△) state1 state2