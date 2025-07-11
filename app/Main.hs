module Main (main) where

import Lib

main :: IO ()
main = do
	putStrLn "=== Puriq V0.1.0 ==="
	putStrLn "Intérprete de lenguaje de programación en español"
	putStrLn ""
	someFunc
	putStrLn ""
	putStrLn "Usa 'puriw --ayuda' para más información"

