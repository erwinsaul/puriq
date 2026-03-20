module Puriq.Repl
   ( iniciarRepl
   ) where

import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as Map
import Puriq.Parser (parseFromString, showParseError)
import Puriq.Evaluator (evaluar, EvalError(..), Entorno)
import Puriq.Types (Valor(..))

iniciarRepl :: IO ()
iniciarRepl = do
    putStrLn "=== Puriq REPL V0.1.0 ==="
    putStrLn "Puedes probar expresiones matemáticas simples. Usa 'salir' para terminar."
    putStrLn ""
    bucleRepl Map.empty


bucleRepl :: Entorno -> IO ()
bucleRepl env = do
    -- Mostrar el prompt
    putStr "puriq> "
    hFlush stdout

    -- Leer entrada (READ
    entrada <- getLine
    if entrada == "Salir" || entrada == "salir"
        then putStrLn "Adiós!"
        else do
            env' <- procesarEntrada env entrada
            bucleRepl env'
 

procesarEntrada :: Entorno -> String -> IO Entorno
procesarEntrada env entrada 
    -- Ignorar las líneas vacías
    | null entrada = return env
    | otherwise = 
        -- EVAL: Parsear y evaluar
        case parseFromString entrada of
            Left errorParser -> do
                -- Mostrar error de parsing
                putStrLn $ "Error: " ++ showParseError errorParser
                return env
            Right expresion ->
                -- Evaluar la expresión pareseada
                case evaluar env expresion of
                    Left errorEval -> do
                        -- Mostrar error de evaluación
                        putStrLn $ "Error: " ++ mostrarErrorEval errorEval
                        return env
                    Right (resultado, env') -> do
                        -- PRINT: Mostrar resultado
                        putStrLn $ mostrarValor resultado
                        return env'

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
mostrarErrorEval (ErrorTipoIncompatible msg) = "Tipo incompatible: " ++ msg
mostrarErrorEval (ErrorVariableNoDefinida var) = "Variable no definida: " ++ var