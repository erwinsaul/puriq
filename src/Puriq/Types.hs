module Puriq.Types where

--Tipos de datos básicos del Lenguaje Puriq
data Valor
	= Entero Int
	| Decimal Double
	| Cadena String
	| Booleano Bool
	| Nulo
	deriving (Show, Eq)

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
	deriving (Show, Eq)
