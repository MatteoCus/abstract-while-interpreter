module Main (main) where

import Stm
import Parser
import ConcreteInterpreter (exec, State)
import Data.Map (Map)
import qualified Data.Map as Map
import IntervalDomain (Interval (..), AbstractDomain (..), Infinitable (..))

run :: [Stm] -> State -> State
run xs state = foldl (flip exec) state xs

emptyInterval :: Interval
emptyInterval = Empty

testInterval :: Interval
testInterval = Interval (Regular (-4)) (Regular (3))

test2Interval :: Interval
test2Interval = Interval (Regular 2) (Regular 3)

test3Interval :: Interval
test3Interval = Interval (Regular 3) (Regular 6)

test4Interval :: Interval
test4Interval = Interval (Regular 1) (Regular 4)

main :: IO ()
main = do       let test = testInterval IntervalDomain./ test2Interval
                print test

















        -- result <- parseFile "./app/test.txt"
        -- case result
        --         of Left e -> putStrLn $ "Parsing error: " ++ e
        --            Right ast -> do putStrLn "Success!"
        --                            print ast
        --                            let finalState = run ast Map.empty
        --                            print $ Map.lookup "y" finalState