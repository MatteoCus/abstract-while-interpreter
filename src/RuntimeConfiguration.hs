module RuntimeConfiguration (RuntimeConfig(..), toggleWidening, toggleNarrowing, setM, setN, setFileToAnalyze) where
import {-# SOURCE #-} IntervalDomain (Infinitable (..))

data RuntimeConfig = RuntimeConfig {fileToAnalyze :: String, intervalBounds :: (Infinitable Integer, Infinitable Integer), enableWidening :: Bool, enableNarrowing :: Bool}

instance Show RuntimeConfig where
    show configuration = do
                        let file = "\n- File: " ++ fileToAnalyze configuration
                        let widening = "\n- Widening: " ++ if enableWidening configuration then "ON" else "OFF"
                        let narrowing = "\n- Narrowing: " ++ if enableNarrowing configuration then "ON" else "OFF"

                        let interval = "- Interval: " ++ case intervalBounds configuration
                                                            of  (Regular l, PositiveInfinity ) -> "[" ++ show l ++ "," ++ show PositiveInfinity ++ ")"
                                                                (NegativeInfinity, Regular u ) -> "(" ++ show NegativeInfinity ++ "," ++ show u ++ "]"
                                                                (NegativeInfinity, PositiveInfinity ) -> "(" ++ show NegativeInfinity ++ "," ++ show PositiveInfinity ++ ")"
                                                                (l, u ) -> "[" ++ show l ++ "," ++ show u ++ "]"
                        interval ++ widening ++ narrowing ++ file
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