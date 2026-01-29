module Main (main) where

import Parser
import CFG (buildCFG)
import AbstractInterpreter (interpret)
import qualified Data.Set as Set
import PrettyPrint (prettyPrintStm, prettyPrintCFG, prettyPrintStates)
import RuntimeConfiguration (RuntimeConfig(..))

main ::  IO ()
main = do
        let config = RuntimeConfig {intervalBounds = (-100, 100), enableWidening = False, enableNarrowing = False}
        result <- parseFile "./app/test.txt" config
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
                                let states = interpret graph config
                                
                                -- Print results
                                putStrLn "=== Analysis Results ==="
                                putStrLn $ prettyPrintStates states
                                putStrLn ""
                                