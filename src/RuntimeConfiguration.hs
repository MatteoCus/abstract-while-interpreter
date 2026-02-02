module RuntimeConfiguration (RuntimeConfig(..), toggleWidening, toggleNarrowing, setM, setN, setFileToAnalyze, addBinding, removeBinding) where
import Infinitable (Infinitable (..))
import AbstractInterpreter.State (State, SmashedBottom (..))

import qualified Data.Map as Map
import {-# SOURCE #-} IntervalDomain (Interval)
import {-# SOURCE #-} PrettyPrint (prettyState)

data RuntimeConfig = RuntimeConfig {
    startingConfiguration :: State,
    fileToAnalyze :: String,
    intervalBounds :: (Infinitable Integer, Infinitable Integer),
    enableWidening :: Bool,
    enableNarrowing :: Bool
}

instance Show RuntimeConfig where
    show configuration = do
                        let file = "\n- File: " ++ fileToAnalyze configuration
                        let widening = "\n- Widening: " ++ if enableWidening configuration then "ON" else "OFF"
                        let narrowing = "\n- Narrowing: " ++ if enableNarrowing configuration then "ON" else "OFF"
                        let startConf = "\n- Starting configuration (user-defined bindings): " ++ prettyState (startingConfiguration configuration)
                        let interval = "- Interval: " ++ case intervalBounds configuration
                                                            of  (Regular l, PositiveInfinity ) -> "[" ++ show l ++ "," ++ show PositiveInfinity ++ ")"
                                                                (NegativeInfinity, Regular u ) -> "(" ++ show NegativeInfinity ++ "," ++ show u ++ "]"
                                                                (NegativeInfinity, PositiveInfinity ) -> "(" ++ show NegativeInfinity ++ "," ++ show PositiveInfinity ++ ")"
                                                                (l, u ) -> "[" ++ show l ++ "," ++ show u ++ "]"
                        interval ++ widening ++ narrowing ++ startConf ++ file

toggleWidening :: RuntimeConfig -> RuntimeConfig
toggleWidening config = config {enableWidening = not (enableWidening config)}

toggleNarrowing :: RuntimeConfig -> RuntimeConfig
toggleNarrowing config = config {enableNarrowing = not (enableNarrowing config)}

setM :: RuntimeConfig -> Infinitable Integer -> RuntimeConfig
setM config m = config {intervalBounds = (m, n)}
                where (_,n) = intervalBounds config

setN :: RuntimeConfig -> Infinitable Integer -> RuntimeConfig
setN config n = config {intervalBounds = (m, n)}
                where (m,_) = intervalBounds config

setFileToAnalyze :: String -> RuntimeConfig -> RuntimeConfig
setFileToAnalyze file config = config {fileToAnalyze = file}

addBinding :: String -> Interval -> RuntimeConfig -> RuntimeConfig
addBinding x interval config = case startingConfiguration config
                             of Left SmashedBottom -> config {startingConfiguration = Right $ Map.singleton x interval}
                                Right state -> config {startingConfiguration = Right $ Map.union (Map.singleton x interval) state }

removeBinding :: String -> RuntimeConfig -> RuntimeConfig
removeBinding x config = case startingConfiguration config
                             of Left SmashedBottom -> config
                                Right state ->  config {startingConfiguration = Right newState}
                                                where newState = Map.delete x state