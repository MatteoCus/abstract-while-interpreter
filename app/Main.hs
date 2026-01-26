module Main (main) where

import Parser
import CFG (buildCFG, findLoopLabels)
import AbstractInterpreter (interpret)
import qualified Data.Set as Set
import PrettyPrint (prettyPrintStm, prettyPrintCFG, prettyPrintStates)
-- import Data.Map (Map)
-- import qualified Data.Map as Map
-- import IntervalDomain (Interval (..), AbstractDomain (..), Infinitable (..))


-- emptyInterval :: Interval
-- emptyInterval = Empty

-- testInterval :: Interval
-- testInterval = Interval (Regular (-4)) (Regular (3))

-- test2Interval :: Interval
-- test2Interval = Interval (Regular 2) (Regular 3)

-- test3Interval :: Interval
-- test3Interval = Interval (Regular 3) (Regular 6)

-- test4Interval :: Interval
-- test4Interval = Interval (Regular 1) (Regular 4)

main ::  IO ()
main = do
        result <- parseFile "./app/test.txt"
        case result
                of Left e -> putStrLn $ "Parsing error: " ++ e
                   Right ast -> do -- Print AST
                                putStrLn "=== AST ==="
                                putStrLn $ prettyPrintStm ast
                                putStrLn ""
                                
                                -- Build CFG
                                let graph = buildCFG ast (Set.empty, 0, 0, []) 0
                                
                                -- Print CFG
                                putStrLn "=== CFG ==="
                                putStrLn $ prettyPrintCFG graph
                                putStrLn ""
                                
                                -- Run analysis
                                let states = interpret graph
                                
                                -- Print results
                                putStrLn "=== Analysis Results ==="
                                putStrLn $ prettyPrintStates states
                                putStrLn ""
                                