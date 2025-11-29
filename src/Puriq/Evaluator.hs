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

