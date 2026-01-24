module AbstractInterpreter where
import qualified Data.Map as Map
import CFG (Command (..))
import Exp (AExp (..), Comparison (..))
import IntervalDomain (Interval (..), Infinitable (..), AbstractDomain (..))
import qualified Data.Maybe

data SmashedBottom = SmashedBottom

type State = Either SmashedBottom (Map.Map String Interval)

evalAExp :: AExp -> State -> Interval
evalAExp _ (Left _) = Empty
evalAExp (ConstantRange l u) _ = Interval (Regular l) (Regular u)
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

exec :: Command -> State -> State
exec _ (Left SmashedBottom) = Left SmashedBottom
exec (CAssign var aExp) (Right state) = case evalAExp aExp (Right state)
                                of Empty -> Left SmashedBottom
                                   (Interval low maxim) -> Right $ Map.insert var (Interval low maxim) state
exec _ state = state