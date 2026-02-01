module Main (main) where
import REPL (repl, headerMessage, config)

main :: IO ()
main = do
        putStrLn headerMessage
        >> repl config
