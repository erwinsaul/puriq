module Main (main) where

import Puriq.Repl (iniciarRepl)

main :: IO ()
main = do
	putStrLn "=== Puriq V0.1.0 ==="
	putStrLn "Intérprete de lenguaje de programación en español"
	putStrLn ""
	someFunc
	putStrLn ""
	putStrLn "Usa 'puriq --ayuda' para más información"

