module Puriq.Evaluator
  ( evaluar
  , EvalError(..)
  , Entorno 
  ) where

import Puriq.Types (Valor(..), Expresion(..))
import qualified Data.Map.Strict as Map

type Entorno = Map.Map String Valor

data EvalError
  = ErrorDivisionPorCero
  | ErrorOperadorDesconocido String
  | ErrorTipoIncompatible String
  | ErrorVariableNoDefinida String
  deriving (Show, Eq)

evaluar :: Entorno -> Expresion -> Either EvalError (Valor, Entorno)
evaluar env (ExpLiteral v) = Right (v, env)
evaluar env (ExpVariable nombre) =
  case Map.lookup nombre env of
    Just v -> Right (v, env)
    Nothing -> Left (ErrorVariableNoDefinida nombre)
evaluar env (ExpAsignacion nombre expr) = do
  (v, env') <- evaluar env expr
  let env'' = Map.insert nombre v env'
  return (v, env'')
evaluar env (ExpBinaria op izq der) = do
  (valorIzp, env') <- evaluar env izq
  (valorDer, env'') <- evaluar env' der
  resultado <- aplicarOperadorBinario op valorIzp valorDer
  return (resultado, env'')
evaluar env (ExpUnaria op expr) = do
  (valor, env') <- evaluar env expr
  resultado <- aplicarOperadorUnario op valor
  return (resultado, env')
evaluar env(ExpSi condicion entonces mSino) = do
  (valCond, env') <- evaluar env condicion
  case valCond of
    Booleano True -> evaluar env' entonces
    Booleano False -> case mSino of
      Just siNo -> evaluar env' siNo
      Nothing -> Right (Nulo, env')
    _ -> Left (ErrorTipoIncompatible "La condicion del 'si' debe ser booleana")
    
-- Operacion Suma
aplicarOperadorBinario :: String -> Valor -> Valor -> Either EvalError Valor
aplicarOperadorBinario "+" (Entero a) (Entero b) = Right (Entero (a+b))
aplicarOperadorBinario "+" (Decimal a) (Decimal b) = Right (Decimal (a+b))
aplicarOperadorBinario "+" (Entero a) (Decimal b) = Right (Decimal (fromIntegral a + b))
aplicarOperadorBinario "+" (Decimal a) (Entero b) = Right (Decimal (a + fromIntegral b))
aplicarOperadorBinario "+" (Cadena a) (Cadena b) = Right (Cadena (a ++ b))

-- Operacion Resta
aplicarOperadorBinario "-" (Entero a) (Entero b) = Right (Entero (a-b))
aplicarOperadorBinario "-" (Decimal a) (Decimal b) = Right (Decimal (a-b))
aplicarOperadorBinario "-" (Entero a) (Decimal b) = Right (Decimal (fromIntegral a - b))
aplicarOperadorBinario "-" (Decimal a) (Entero b) = Right (Decimal (a-fromIntegral b))

-- Operación Multiplicación 
aplicarOperadorBinario "*" (Entero a) (Entero b) = Right (Entero (a*b))
aplicarOperadorBinario "*" (Decimal a) (Decimal b) = Right (Decimal (a*b))
aplicarOperadorBinario "*" (Entero a) (Decimal b) = Right (Decimal (fromIntegral a * b))
aplicarOperadorBinario "*" (Decimal a) (Entero b) = Right (Decimal (a*fromIntegral b))

-- Operación División
aplicarOperadorBinario "/" _ (Entero 0) = Left ErrorDivisionPorCero
aplicarOperadorBinario "/" _ (Decimal 0.0) = Left ErrorDivisionPorCero
aplicarOperadorBinario "/" (Entero a) (Entero b) = Right (Decimal (fromIntegral a / fromIntegral b))
aplicarOperadorBinario "/" (Decimal a) (Decimal b) = Right (Decimal (a/b))
aplicarOperadorBinario "/" (Entero a) (Decimal b) = Right (Decimal (fromIntegral a / b))
aplicarOperadorBinario "/" (Decimal a) (Entero b) = Right (Decimal (a / fromIntegral b))

-- Operadores lógicos
aplicarOperadorBinario "y" (Booleano a) (Booleano b) = Right (Booleano (a && b))
aplicarOperadorBinario "o" (Booleano a) (Booleano b) = Right (Booleano (a || b))

-- Operdaores de comparacion numérica (Entero Vs Entero)
aplicarOperadorBinario "==" (Entero a) (Entero b) = Right (Booleano (a == b))
aplicarOperadorBinario "!=" (Entero a) (Entero b) = Right (Booleano (a /= b))
aplicarOperadorBinario "<"  (Entero a) (Entero b) = Right (Booleano (a < b))
aplicarOperadorBinario ">"  (Entero a) (Entero b) = Right (Booleano (a > b))
aplicarOperadorBinario "<=" (Entero a) (Entero b) = Right (Booleano (a <= b))
aplicarOperadorBinario ">=" (Entero a) (Entero b) = Right (Booleano (a >= b))

-- Operadores de comparacion numérica (Decimal Vs Decimal)
aplicarOperadorBinario "==" (Decimal a) (Decimal b) = Right (Booleano (a == b))
aplicarOperadorBinario "!=" (Decimal a) (Decimal b) = Right (Booleano (a /= b))
aplicarOperadorBinario "<"  (Decimal a) (Decimal b) = Right (Booleano (a < b))
aplicarOperadorBinario ">"  (Decimal a) (Decimal b) = Right (Booleano (a > b))
aplicarOperadorBinario "<=" (Decimal a) (Decimal b) = Right (Booleano (a <= b))
aplicarOperadorBinario ">=" (Decimal a) (Decimal b) = Right (Booleano (a >= b))

-- Operadores de comparacion numerica (Entero VS Decimal y viceversa)
aplicarOperadorBinario "==" (Entero a) (Decimal b) = Right (Booleano (fromIntegral a == b))
aplicarOperadorBinario "!=" (Entero a) (Decimal b) = Right (Booleano (fromIntegral a /= b))
aplicarOperadorBinario "<"  (Entero a) (Decimal b) = Right (Booleano (fromIntegral a < b))
aplicarOperadorBinario ">"  (Entero a) (Decimal b) = Right (Booleano (fromIntegral a > b))
aplicarOperadorBinario "<=" (Entero a) (Decimal b) = Right (Booleano (fromIntegral a <= b))
aplicarOperadorBinario ">=" (Entero a) (Decimal b) = Right (Booleano (fromIntegral a >= b))

aplicarOperadorBinario "==" (Decimal a) (Entero b) = Right (Booleano (a == fromIntegral b))
aplicarOperadorBinario "!=" (Decimal a) (Entero b) = Right (Booleano (a /= fromIntegral b))
aplicarOperadorBinario "<"  (Decimal a) (Entero b) = Right (Booleano (a < fromIntegral b))
aplicarOperadorBinario ">"  (Decimal a) (Entero b) = Right (Booleano (a > fromIntegral b))
aplicarOperadorBinario "<=" (Decimal a) (Entero b) = Right (Booleano (a <= fromIntegral b))
aplicarOperadorBinario ">=" (Decimal a) (Entero b) = Right (Booleano (a >= fromIntegral b))

-- Igualdad entre cadenas
aplicarOperadorBinario "==" (Cadena a) (Cadena b) = Right (Booleano (a == b))
aplicarOperadorBinario "!=" (Cadena a) (Cadena b) = Right (Booleano (a /= b))

-- Igualdad entre booleanos
aplicarOperadorBinario "==" (Booleano a) (Booleano b) = Right (Booleano (a == b))
aplicarOperadorBinario "!=" (Booleano a) (Booleano b) = Right (Booleano (a /= b))

-- En caso de que el operador no sea reconocido
aplicarOperadorBinario op (Cadena _) (Cadena _) = Left (ErrorTipoIncompatible ("Operador '" ++ op ++ "' no válido para cadenas"))
aplicarOperadorBinario op (Booleano _) (Booleano _) = Left (ErrorTipoIncompatible ("Operador '" ++ op ++ "' no válido para booleanos"))
aplicarOperadorBinario op _ _ = Left (ErrorOperadorDesconocido op)

aplicarOperadorUnario :: String -> Valor -> Either EvalError Valor
aplicarOperadorUnario "-" (Entero n) = Right (Entero (-n))
aplicarOperadorUnario "-" (Decimal n) = Right (Decimal (-n))
aplicarOperadorUnario "no" (Booleano b) = Right (Booleano (not b))

aplicarOperadorUnario op _ = Left (ErrorOperadorDesconocido op)







