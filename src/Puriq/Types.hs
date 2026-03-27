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
    | TokBooleano Bool
    | TokIdentificador String
    | TokPalabraReservada String
    | TokOperador String
    | TokParenIzq
    | TokParenDer
    | TokIgual
    | TokDosPuntos
    | TokError Char
    | TokFin
    deriving (Show, Eq, Ord)

-- Expresiones del AST
data Expresion
    = ExpLiteral Valor
    | ExpVariable String
    | ExpAsignacion String Expresion
    | ExpBinaria String Expresion Expresion
    | ExpUnaria String Expresion
    | ExpSi Expresion Expresion (Maybe Expresion)
    deriving (Show, Eq, Ord)
