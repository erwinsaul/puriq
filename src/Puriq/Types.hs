module Puriq.Types 
  ( Valor(..)
    , Token(..)
    , Expresion(..)
   )
where

--Tipos de datos básicos del Lenguaje Puriq
data Valor
    = Entero Int
    | Decimal Double
    | Cadena String
    | Booleano Bool
    | Nulo
    deriving (Show, Eq, Ord)

-- Tokens del lexer 
data Token
    = TokEntero Int
    | TokDecimal Double
    | TokCadena String
    | TokBooleando Bool
    | TokIdentificador String
    | TokOperador String
    | TokParenIzq
    | TokParenDer
    | TokFin
    deriving (Show, Eq, Ord)

-- Expresiones del AST
data Expresion
    = ExpLiteral Valor
    | ExpVariable String
    | ExpBinaria String Expresion Expresion
    | ExpUnaria String Expresion
    deriving (Show, Eq, Ord)
