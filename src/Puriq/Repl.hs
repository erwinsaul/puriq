module Puriq.Repl
   ( iniciarRepl
   ) where

import System.IO (hFlush, stdout)
import Puriq.Parser (parseFromString, showParseError)
import Puriq.Evaluator (evaluar, EvalError(..))
import Puriq.Types (Valor(..))

iniciarRepl :: IO ()
iniciarRepl = do
    putStrLn "=== Puriq REPL V0.1.0 ==="
    putStrLn "Puedes probar expresiones matemáticas simples. Usa 'salir' para terminar."
    putStrLn ""
    bucleRepl


bucleRepl :: IO ()
bucleRepl = do
    -- Mostrar el prompt
    putStr "puriq> "
    hFlush stdout

    -- Leer entrada (READ
    entrada <- getLine

    -- Verificar si quiere salir

    if entrada == "Salir" || entrada == "salir"
        then putStrLn "Adiós!"
        else do
            -- Procesar la entrada
            procesarEntrada entrada
            -- Continuar el bucle (LOOP)
            bucleRepl

pocesarEntrada :: String -> IO ()
procesarEntrada entrada 
    -- Ignorar las líneas vacías
    | null entrada = return ()
    | otherwise = 
        -- EVAL: Parsear y evaluar
        case parseFromString entrada of
            Left errorParser ->
                -- Mostrar error de parsing
                putStrLn $ "Error: " ++ ShowParseError errorParser
            Right expresion ->
                -- Evaluar la expresión pareseada
                case evaluar expresion of
                    Left errorEval ->
                        -- Mostrar error de evaluación
                        putStrLn $ "Error: " ++ mostrarErrorEval errorEval
                    Right resultado ->
                        -- PRINT: Mostrar resultado
                        putStrLn $ mostrarValor resultado

-- Mostrar un valor de forma legible
mostrarValor :: Valor -> String
mostrarValor (Entero n) = show n
mostrarValor (Decimal d) = show d
mostrarValor (Cadena s) = s
mostrarValor (Booleano b) = if b then "verdadero" else "falso"
mostrarValor Nulo = "nulo"

-- Mostrar errores de evaluación en español
mostrarErrorEval :: EvalError -> String
mostrarErrorEval ErrorDivisionPorCero = "División por cero"
mostrarErrorEval (ErrorOperadorDesconocido op) = "Operador desconocido: " ++ op
mostrarErrorEval (ErrorTipoIncompatible msg) = "Tipo incompatible" ++ msg
mostrarErrorEval (ErrorVariableNoDefinida var) = "Variable no definida:" ++ var