module AbstractInterpreter.State where
import qualified Data.Map as Map
import {-# SOURCE #-} IntervalDomain (Interval)
data SmashedBottom = SmashedBottom
   deriving (Eq, Show)

type State = Either SmashedBottom (Map.Map String Interval)