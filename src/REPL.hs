module REPL where
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
import Parser (parseFile)
import Control.Monad (unless)

(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < 0     = Nothing
    | otherwise = listToMaybe (drop n xs)


config :: RuntimeConfig
config = RuntimeConfig {fileToAnalyze = baseExampleDirectory ++ "test.txt", intervalBounds = (NegativeInfinity, PositiveInfinity), enableWidening = False, enableNarrowing = False}

baseExampleDirectory :: String
baseExampleDirectory = "./examples/"

loadFailMessage :: String
loadFailMessage = "Please select a valid example file!"

loadSuccessMessage :: String -> String
loadSuccessMessage example = "Example file  " ++ example ++ " loaded."

intervalFailMessage :: String
intervalFailMessage = "Invalid interval!"

intervalSuccessMessage :: Integer -> Integer -> String
intervalSuccessMessage m n= "New interval: [" ++ show m ++ ", " ++ show n ++ "]"

loadMessage :: [String] -> String
loadMessage files = do
                      let start = "Available example files: \n"
                      start ++ loadMessage' files 1
loadMessage' :: [String] -> Int -> String
loadMessage' [] _ = ""
loadMessage' (x : xs) n = show n ++ "): " ++ x ++ "\n" ++ loadMessage' xs (n Prelude.- 1)

loadRepl :: RuntimeConfig -> IO (String, RuntimeConfig)
loadRepl runtimeConfig = do
                    allFiles <- getDirectoryContents baseExampleDirectory
                    let files = filter (`notElem` [".", ".."]) allFiles
                    print_ $ loadMessage files
                    input <- putStr ">load " >> readNumber_
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
                                print_  "New lower bound: "
                                m <- putStr ">interval " >> readNumber_
                                print_  "New upper bound: "
                                n <- putStr ">interval " >> readNumber_
                                result <- intervalEval_ m n runtimeConfig

                                if fst result == intervalFailMessage
                                then print_ (fst result ++ "\n") >> intervalRepl runtimeConfig
                                else return result

intervalEval_ :: Integer -> Integer -> RuntimeConfig -> IO(String, RuntimeConfig)
intervalEval_ m n runtimeConfig = if m > n
                                   then return (intervalFailMessage, runtimeConfig)
                                   else return (intervalSuccessMessage m n, setN (setM runtimeConfig (Regular m)) (Regular n))

read_ :: IO String
read_ = putStr "> "
     >> hFlush stdout
     >> getLine

readNumber_ :: IO Integer
readNumber_ = do
                hFlush stdout >> readLn

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
                                                   where graph = buildCFG ast (Set.empty, 0, 0, []) 0

eval_ ":analyze" runtimeConfig =  do
                                    result <- parseFile runtimeConfig
                                    case result
                                      of  Left e -> return ("Parsing error: " ++ e, runtimeConfig)
                                          Right ast -> return (prettyPrintStates states, runtimeConfig)
                                                      where graph = buildCFG ast (Set.empty, 0, 0, []) 0
                                                            states = interpret graph runtimeConfig
eval_ ":load" runtimeConfig = loadRepl runtimeConfig
eval_ ":interval" runtimeConfig = intervalRepl runtimeConfig

eval_ input runtimeConfig = return ("Unknown command: " ++ input, runtimeConfig)

print_ :: String -> IO ()
print_ = putStrLn

repl :: RuntimeConfig -> IO ()
repl runtimeConfig = do
  input <- read_
  result <- eval_ input runtimeConfig
  unless (input == ":quit") $ print_ (fst result ++ "\n" ) >> repl (snd result)

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
              \                      - (-inf, +inf) as parametric interval, basic interval domain\
              \\n\n\
              \:configuration        Show the content of the analyzer configuration.\
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