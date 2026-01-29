module Main (main) where

import Parser
import CFG (buildCFG, findLoopLabels)
import AbstractInterpreter (interpret)
import qualified Data.Set as Set
import PrettyPrint (prettyPrintStm, prettyPrintCFG, prettyPrintStates)

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
                                