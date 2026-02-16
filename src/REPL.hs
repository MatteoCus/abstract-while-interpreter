module REPL (repl, headerMessage, config) where
import RuntimeConfiguration
import qualified Data.Set as Set
import PrettyPrint
import CFG
import AbstractInterpreter
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import System.Directory (getDirectoryContents)
import Data.Maybe (listToMaybe)
import IntervalDomain hiding ((-))
import Parser (parseFile, integerParser, infinitable)
import Control.Monad (unless)
import Text.Parsec
import qualified Data.Map as Map
import CFG.GraphOperations (freeVariablesCom)
import Data.Set (toList)

(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < 0     = Nothing
    | otherwise = listToMaybe (drop n xs)


config :: RuntimeConfig
config = RuntimeConfig {startingConfiguration = Right Map.empty, fileToAnalyze = baseExampleDirectory ++ "simple-loop.txt", intervalBounds = (NegativeInfinity, PositiveInfinity), enableWidening = False, enableNarrowing = False, descendingSteps = 5}

headerMessage :: String
headerMessage = "Welcome to the abstract interpreter for While language! \
                \\n\nType :help for help"

infoMessage :: String
infoMessage = "The analysis is performed by modeling the program as a Control-Flow Graph and iterating over the directed arcs using a parametrized version of the interval domain. \n\
              \You can configure: \n\
              \       1) The range [m,n] of the interval domain (it could even be infinite); if you set m > n, you'll fallback into the constant propagation domain \n\
              \       2) The use of widening during the analysis (note: if [m,n] finite, the analysis will converge even without widening) \n\
              \       3) The use of narrowing to refine the analysis \n\
              \       4) The example file to analyze, from a list of pre-defined files (contained inside the './examples' directory, which is scanned each time the option is selected) \n\
              \       5) The starting configuration for the analysis (if unspecified, expect ⊤ for the entry label, ⊥ for all the other labels, (-inf, +inf) interval)"

helpMessage :: String
helpMessage = ":load                 Load a While source code from FILE to memory.\n\ 
              \                      Displayed files are stored inside the /examples directory.\
              \\n\n\
              \:analyze              It interprets the latest loaded FILE\
              \\n\n\
              \:ast                  Show the AST of the latest loaded FILE.\
              \\n\n\
              \:cfg                  Show the CFG of the latest loaded FILE.\
              \\n\n\
              \:reset                Reset the analyzer configuration.\n\
              \                      - ⊤ for the entry label \n\
              \                      - ⊥ for all the other labels \n\
              \                      - (-inf, +inf) as parametric interval, basic interval domain\n\
              \                      - './examples/simple-loop.txt' as loaded file\
              \\n\n\
              \:configuration        Show the content of the analyzer configuration.\
              \\n\n\
              \:addBinding           Add a new variable binding to the custom-defined initial configuration \n\
              \                      for the entry label.\
              \\n\n\
              \:removeBinding        Remove a variable binding from the custom-defined initial configuration \n\
              \                      for the entry label.\
              \\n\n\
              \:interval             Update the interval used during analysis.\
              \\n\n\
              \:widening             Toggle usage of widening during analysis.\n\
              \                      By default, widening is off.\
              \\n\n\
              \:narrowing            Toggle usage of narrowing during analysis.\n\
              \                      By default, narrowing is off.\
              \\n\n\
              \:info                 Show infos about this project and the analysis algorithm implemented.\
              \\n\n\
              \:quit                 Quit this REPL.\
              \\n\n\
              \:help                 Show this help message."

baseExampleDirectory :: String
baseExampleDirectory = "./examples/"

loadFailMessage :: String
loadFailMessage = "Please select a valid example file!"

bindingFailMessage :: String
bindingFailMessage = "Please select a valid variable!"

loadSuccessMessage :: String -> String
loadSuccessMessage example = "Example file  " ++ example ++ " loaded."

intervalFailMessage :: String
intervalFailMessage = "Invalid interval!"

constantPropagationMessage :: String
constantPropagationMessage = "\nLower bound is greater than upper bound, the analysis will take place with constant propagation domain!"

intervalSuccessMessage :: Infinitable Integer -> Infinitable Integer -> String
intervalSuccessMessage m n= "New interval: [" ++ show m ++ ", " ++ show n ++ "]"

loadMessage :: [String] -> String
loadMessage files = do
                      let start = "Type '-1' to exit the file loading. \n\n\
                                  \Available example files: \n"
                      start ++ showArrayStrings' files 1

bindingMessage :: [String] -> String
bindingMessage variables = do
                      let start = "Type '-1' to exit the variable selection. \n\n\
                                  \Available variables: \n"
                      start ++ showArrayStrings' variables 1

showArrayStrings' :: [String] -> Int -> String
showArrayStrings' [] _ = ""
showArrayStrings' (x : xs) n = show n ++ "): " ++ x ++ "\n" ++ showArrayStrings' xs (n Prelude.+ 1)

loadRepl :: RuntimeConfig -> IO (String, RuntimeConfig)
loadRepl runtimeConfig = do
                    allFiles <- getDirectoryContents baseExampleDirectory
                    let files = filter (`notElem` [".", ".."]) allFiles
                    print_ $ loadMessage files
                    selected <- putStr ">load " >> read_
                    case parse integerParser ""  selected of
                      Left _ -> print_ "expecting integer \n" >> loadRepl runtimeConfig
                      Right input -> do
                                      result <- loadEval_ input files runtimeConfig

                                      if input /= -1 && fst result == loadFailMessage
                                        then print_ (fst result ++ "\n" ) >> loadRepl runtimeConfig
                                        else if input == -1
                                              then return ("Quit from file loading", runtimeConfig)
                                              else return result

loadEval_ :: Integer -> [String] -> RuntimeConfig -> IO (String, RuntimeConfig)
loadEval_ index files runtimeConfig = case files !? (fromInteger index-1)
                                        of  Nothing -> return (loadFailMessage, runtimeConfig)
                                            Just file -> return (loadSuccessMessage file, setFileToAnalyze (baseExampleDirectory ++ file) runtimeConfig)

intervalRepl :: RuntimeConfig -> IO (String, RuntimeConfig)
intervalRepl runtimeConfig = do
                                print_  "If lower bound is greater than upper bound, fallback to constant propagation domain. \n\n\
                                        \New lower bound (-inf, +inf for infinite bounds): "
                                lowerB <- putStr ">interval " >> read_
                                case parse infinitable ""  lowerB of
                                  Left _ -> print_ "expecting integer or infinite \n" >> intervalRepl runtimeConfig
                                  Right m -> do
                                                print_  "New upper bound (-inf, +inf for infinite bounds): "
                                                upperB <- putStr ">interval " >> read_
                                                case parse infinitable ""  upperB of
                                                  Left _ -> print_ "expecting integer or infinite \n" >> intervalRepl runtimeConfig
                                                  Right n -> do intervalEval_ m n runtimeConfig

intervalEval_ :: Infinitable Integer -> Infinitable Integer -> RuntimeConfig -> IO(String, RuntimeConfig)
intervalEval_ m n runtimeConfig = if m > n
                                   then return (constantPropagationMessage, setN (setM runtimeConfig m) n)
                                   else return (intervalSuccessMessage m n, setN (setM runtimeConfig m) n)

addBindingRepl :: RuntimeConfig -> IO (String, RuntimeConfig)
addBindingRepl runtimeConfig = do
  result <- parseFile runtimeConfig
  case result of
    Left e -> return ("Parsing error while trying to infer variables for binding: " ++ e, runtimeConfig)
    Right ast -> handleValidAst ast

  where
    handleValidAst ast = do
      let arcs = getArcs ast
      let freeVar = extractFreeVariables arcs
      if null freeVar then return ("", runtimeConfig)
      else do
        print_ $ bindingMessage freeVar
        selected <- promptUser ">binding "
        handleSelection freeVar selected

    getArcs ast = 
      let (_, _, _, arcs) = buildCFG ast (Set.empty, 0, 0, Set.empty) 0
      in arcs

    extractFreeVariables = toList . foldr (Set.union . (\(_, cm, _) -> freeVariablesCom cm)) Set.empty

    handleSelection freeVar selected =
      case parse integerParser "No variables to bind" selected of
        Left _ -> print_ "expecting integer \n" >> addBindingRepl runtimeConfig
        Right input -> processInput freeVar input

    processInput freeVar input
      | input == -1 = return ("Quit from adding variable binding", runtimeConfig)
      | otherwise = selectVariable freeVar input

    selectVariable freeVar input =
      case freeVar !? (fromInteger input - 1) of
        Nothing -> print_ bindingFailMessage >> addBindingRepl runtimeConfig
        Just var -> getBounds var

    getBounds var = do
      lowerB <- promptBound "Lower"
      case lowerB of
        Nothing -> print_ "expecting integer or infinite \n" >> eval_ ":configuration" runtimeConfig
        Just l -> do
          upperB <- promptBound "Upper"
          case upperB of
            Nothing -> print_ "expecting integer or infinite \n" >> eval_ ":configuration" runtimeConfig
            Just u -> addBindingEval_ var l u runtimeConfig

    promptBound boundType = do
      print_ $ boundType ++ " bound (-inf, +inf for infinite bounds): "
      input <- promptUser ">binding "
      return $ either (const Nothing) Just (parse infinitable "" input)

    promptUser prompt = putStr prompt >> read_

addBindingEval_ :: String -> Infinitable Integer -> Infinitable Integer -> RuntimeConfig -> IO(String, RuntimeConfig)
addBindingEval_ var l u runtimeConfig
  | l > u = return (intervalFailMessage, runtimeConfig)
  | l > n || u < m = return ("The binding interval is not included in the analysis interval!", runtimeConfig)
  | otherwise = do  let updatedConf = addBinding var (Interval l u) runtimeConfig
                    return (show updatedConf, updatedConf)
  where
    (m, n) = intervalBounds runtimeConfig

removeBindingRepl :: RuntimeConfig -> IO (String, RuntimeConfig)
removeBindingRepl runtimeConfig = 
  case startingConfiguration runtimeConfig of
    Left _ -> return ("No variables to unbind", runtimeConfig)
    Right state -> handleRemoveBinding (Map.keys state)

  where
    handleRemoveBinding freeVar = do
      print_ $ bindingMessage freeVar
      selected <- promptUser ">binding "
      processSelection freeVar selected

    processSelection freeVar selected =
      case parse integerParser "" selected of
        Left _ -> print_ "expecting integer \n" >> removeBindingRepl runtimeConfig
        Right input -> handleInput freeVar input

    handleInput freeVar input
      | input == -1 = return ("Quit from removing variable binding", runtimeConfig)
      | otherwise = selectVariableToRemove freeVar input

    selectVariableToRemove freeVar input =
      case freeVar !? (fromInteger input - 1) of
        Nothing -> print_ bindingFailMessage >> removeBindingRepl runtimeConfig
        Just var -> removeBindingEval_ var runtimeConfig

    promptUser prompt = putStr prompt >> read_

removeBindingEval_ :: String -> RuntimeConfig -> IO (String, RuntimeConfig)
removeBindingEval_ var runtimeConfig = do
                                          let updatedConf = removeBinding var runtimeConfig
                                          return (show updatedConf, updatedConf)

read_ :: IO String
read_ = hFlush stdout
     >> getLine

eval_ :: String -> RuntimeConfig -> IO (String, RuntimeConfig)
eval_ ":help" runtimeConfig = return (helpMessage, runtimeConfig)
eval_ ":info" runtimeConfig = return (infoMessage, runtimeConfig)
eval_ ":configuration" runtimeConfig = return (show runtimeConfig, runtimeConfig)
eval_ ":widening" runtimeConfig = if not $ enableWidening runtimeConfig
                                  then return ("Widening: ON", toggleWidening runtimeConfig)
                                  else return ("Widening: OFF", toggleWidening runtimeConfig)
eval_ ":narrowing" runtimeConfig = if not $ enableNarrowing runtimeConfig
                                  then return ("Narrowing: ON", toggleNarrowing runtimeConfig)
                                  else return ("Narrowing: OFF", toggleNarrowing runtimeConfig)
eval_ ":reset" _ = return ("Configuration reset done. \n\n" ++ show config, config)
eval_ ":ast" runtimeConfig =  do
                                result <- parseFile runtimeConfig
                                case result
                                  of  Left e -> return ("Parsing error: " ++ e, runtimeConfig)
                                      Right ast -> return (prettyPrintStm ast, runtimeConfig)

eval_ ":cfg" runtimeConfig =  do
                                result <- parseFile runtimeConfig
                                case result
                                  of  Left e -> return ("Parsing error: " ++ e, runtimeConfig)
                                      Right ast -> return (prettyPrintCFG graph, runtimeConfig)
                                                   where graph = buildCFG ast (Set.empty, 0, 0, Set.empty) 0

eval_ ":analyze" runtimeConfig =  do
                                    result <- parseFile runtimeConfig
                                    case result
                                      of  Left e -> return ("Parsing error: " ++ e, runtimeConfig)
                                          Right ast -> return (prettyPrintStates states, runtimeConfig)
                                                      where graph = buildCFG ast (Set.empty, 0, 0, Set.empty) 0
                                                            states = interpret graph runtimeConfig
eval_ ":load" runtimeConfig = loadRepl runtimeConfig
eval_ ":interval" runtimeConfig = intervalRepl runtimeConfig
eval_ ":removeBinding" runtimeConfig = removeBindingRepl runtimeConfig
eval_ ":addBinding" runtimeConfig = addBindingRepl runtimeConfig
eval_ input runtimeConfig = return ("Unknown command: " ++ input, runtimeConfig)

print_ :: String -> IO ()
print_ = putStrLn

repl :: RuntimeConfig -> IO ()
repl runtimeConfig = do
  input <- putStr "> " >> read_
  result <- eval_ input runtimeConfig
  unless (input == ":quit") $ print_ (fst result ++ "\n" ) >> repl (snd result)