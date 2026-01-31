module Main (main) where
import REPL

main :: IO ()
main = do
        putStrLn headerMessage
        >> repl config
