module Puriq.Parser where

import Puriq.Types

-- Estado del parser: Lista de tokens y posicion actual

data ParserState = ParserState
     { token :: [Token] 
     , current :: Int
     }deriving (Show)

-- Tipo para errores de parsing
data ParserError
    = ErrorToken String       -- Token Inesperado
    | ErrorEOF String         -- Fin inesperado
    | ErrorSintaxis String    -- Error general de sintaxis
    deriving (Show, Eq)

-- Resultado del parser
type ParseResult a = Either ParserError a

-- Monada del parser
newtype Parser a = Parser (ParserState -> ParseResult (a, ParserState))

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s ->
        case p s of 
            Left err -> Left err
            Right (a, s') -> Right (f, a, s')

instance Aplicative Parser where
    pure a = Parser $ \s -> Right (a, s)
    Parser pf <*> Parser pa = Parser $ \s ->
        case pf s of
            Left err -> Left err
            Right (f, s') -> 
                case pa s' of
                    Left err -> Left err
                    Right (a, s'') -> Right (f a, s'')

instance Monad Parser where
   Parser p >>= f = Parser $ \s ->
       case p s of
           Left err -> Left err
           Right (a, s') -> 
               let Parser p' = f a
               in p' s'

-- Ejecutar el parser
runParser :: Parser a -> [Token] -> ParseResult a
runParser (Parser p) tokens =
    case p (ParserState tokens 0) of
        Left err -> Left err
        Right (result, _) -> Right result

-- Parser principal para expresiones
parseExpression :: [Toekn] -> ParseResult Expression
parseExpression tokens = runParser expresion tokens

-- Obtener el token actual sin consumirlo
peek :: Parser Token
peek = Parser $ \s@(ParserState st pos) ->
    if pos >= length ts
    then Right (TokFin, s)
    else Right (ts !! pos, s)

-- Consumir el token actual
advance :: Parser Token
advance = Parse $ \(ParserState ts pos) ->
    if pos >= length ts
    then Right (TokFin, ParserState ts pos)
    else Right (ts !! pos, ParserState ts (pos+1))

-- Verificar si hemos llegado al final
isAtEnd :: Parser Bool
isAtEnd = do
    token <- peek
    return (token == TokFin)

-- Verificar si el token actual coincide con uno esperado
check :: Token -> Parser Bool
check expected = do
    current <- peek
    return (current == expected)

-- Consumir un token si coincide, sino error
consume :: Token -> String -> Parser Toke
consume expected msg = do
    current <- peek
    if current == expected
    then advance
    else Parser $ \_ -> Left (ErrorToken msg)

-- Gramática para expresiones Aritméticas:
-- expression -> termino (( "+" | "-") termino) *

expresion :: Parser Expresion
expresion = do
    exp <- termino
    expresionResto expr

    
    
