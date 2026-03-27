module Puriq.Repl
   ( iniciarRepl
   , procesarEntrada
   ) where

import System.Console.Haskeline
    (InputT
    , defaultSettings
    , getInputLine
    , outputStrLn
    , runInputT
    )
import qualified Data.Map.Strict as Map
import Puriq.Parser (parseFromString, showParseError)
import Puriq.Evaluator (evaluar, EvalError(..), Entorno)
import Puriq.Types (Valor(..))

iniciarRepl :: IO ()
iniciarRepl = runInputT defaultSettings $ do
    outputStrLn "=== Puriq REPL V0.1.0 ==="
    outputStrLn "Puedes probar expresiones matemáticas simples. Usa 'salir' para terminar."
    outputStrLn ""
    bucleRepl Map.empty


bucleRepl :: Entorno -> InputT IO()
bucleRepl env = do
    mEntrada <- getInputLine "puriq> "
    case mEntrada of
        Nothing -> outputStrLn "Adiós!"
        Just entrada ->
            if entrada == "Salir" || entrada == "salir"
                then outputStrLn "Adiós!"
                else do
                    env' <- procesarEntrada env entrada
                    bucleRepl env'

procesarEntrada :: Entorno -> String -> InputT IO Entorno
procesarEntrada env entrada 
    -- Ignorar las líneas vacías
    | null entrada = return env
    | entrada == "variables" = do
        if Map.null env
            then outputStrLn "(no hay variables definidas)"
            else mapM_ mostrarVariable (Map.toAscList env)
        return env
    | otherwise = 
        -- EVAL: Parsear y evaluar
        case parseFromString entrada of
            Left errorParser -> do
                -- Mostrar error de parsing
                outputStrLn $ "Error: " ++ showParseError errorParser
                return env
            Right expresion ->
                -- Evaluar la expresión pareseada
                case evaluar env expresion of
                    Left errorEval -> do
                        -- Mostrar error de evaluación
                        outputStrLn $ "Error: " ++ mostrarErrorEval errorEval
                        return env
                    Right (resultado, env') -> do
                        -- PRINT: Mostrar resultado
                        outputStrLn $ mostrarValor resultado
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

-- Muestra una variable con su nombre y valor
mostrarVariable :: (String, Valor) -> InputT IO()
mostrarVariable (nombre, valor) = 
    outputStrLn $ " " ++ nombre ++ " = " ++ mostrarValor valor

