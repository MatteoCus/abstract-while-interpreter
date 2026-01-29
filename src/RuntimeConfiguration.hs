module RuntimeConfiguration (RuntimeConfig(..), toggleWidening, toggleNarrowing, setM, setN) where


data RuntimeConfig = RuntimeConfig { intervalBounds :: (Integer, Integer), enableWidening :: Bool, enableNarrowing :: Bool}

toggleWidening :: RuntimeConfig -> RuntimeConfig
toggleWidening config = config {enableWidening = not (enableWidening config)}

toggleNarrowing :: RuntimeConfig -> RuntimeConfig
toggleNarrowing config = config {enableNarrowing = not (enableNarrowing config)}

setM :: RuntimeConfig -> Integer -> RuntimeConfig
setM config m = config {intervalBounds = (m, n)}
                where (_,n) = intervalBounds config 

setN :: RuntimeConfig -> Integer -> RuntimeConfig
setN config n = config {intervalBounds = (m, n)}
                where (m,_) = intervalBounds config 