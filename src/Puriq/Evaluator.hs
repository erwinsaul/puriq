module Puriq.Evaluator
  ( evaluar
  , EvalError(..)
  ) where

import Puriq.Types (Valor(..), Expresion(..))

data EvalError
  = ErrorDivisionPorCero
  | ErrorOperadorDesconocido String
  | ErrorTipoIncompatible String
  | ErrorVariableNoDefinida String
  deriving (Show, Eq)

evaluar :: Expresion -> Either EvalError Valor
evaluar (ExpLiteral v) = Right v
evaluar (ExpBinaria op izq der) = do
  valorIzq <- evaluar izq
  valorDer <- evaluar der
  aplicarOperadorBinario op valorIzq valorDer
evaluar (ExpUnaria op expr) = do
  valor <- evaluar expr
  aplicarOperadorUnario op valor
evaluar (ExpVariable nombre) = do
  Left (ErrorVariableNoDefinida nombre)

-- Operacion Suma
aplicarOperadorBinario :: String -> Valor -> Valor -> Either EvalError Valor
aplicarOperadorBinario "+" (Entero a) (Entero b) = Right (Entero (a+b))
aplicarOperadorBinario "+" (Decimal a) (Decimal b) = Right (Decimal (a+b))
aplicarOperadorBinario "+" (Entero a) (Decimal b) = Right (Decimal (fromIntegral a + b))
aplicarOperadorBinario "+" (Decimal a) (Entero b) = Right (Decimal (a + fromIntegral b))

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

-- En caso de que el operador no sea reconocido
aplicacionOperadorBinario _ _ = Left (ErrorOperadorDesconocido op)

aplicarOperadorUnario :: String -> Valor -> Either EvalError Valor
aplicarOperadorUnario "-" (Entero n) = Right (Entero (-n))
aplicarOperadorUnario "-" (Decimal n) = Right (Decimal (-n))
aplicarOperadorUnario op _ = Left (ErrorOperadorDesconocido op)






