module Main (main) where

import Parser
import CFG (buildCFG, findLoopLabels)
import AbstractInterpreter (interpret)
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

main :: IO ()
main = do
        result <- parseFile "./app/test.txt"
        case result
                of Left e -> putStrLn $ "Parsing error: " ++ e
                   Right ast -> do putStrLn "Success!"
                                   print ast
                                   print ""
                                   let graph = buildCFG ast ([],0,0,[]) 0
                                   print (buildCFG ast ([],0,0,[]) 0)
                                   print ""
                                   print "Interpretation: "
                                   print (interpret graph)