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

expresionResto :: Expresion -> Parser Expresion
expresionResto left = do
    token <- peek
    case token of
        TokOperador "+" -> do
            advance
            right <- termino
            expresionResto (ExpBinaria "+" left right)
        TokOperador "-" -> do
            advance
            right <- termino
            expresionResto (ExpBinaria "-" left right)
        _ -> return left

-- termino -> factor ( ( "*" | "/") factor)*
termino :: Parser Expresion
termino = do
    expr <- factor
    terminoResto expr

terminoResto :: Expresion -> Parser Expresion
terminoResto left = do
    token <- peek
    case token of
        TokOperador "*" -> do
            advance
            right <- factor
            terminoResto (ExpBinaria "*" left right)
        TokOperador "/" -> do
            advance
            right <- factor
            terminoResto (ExpBinaria "/" left right)
        _ -> return left

-- factor -> NUMERO | "(" expresion ")" | "-" factor
factor :: Parser Expresion
factor = do
    token <- peek
    case token of
        TokEntero n -> do
            advance
            return (ExpLiteral (Entero n))
        TokDecimal n -> do
            advance
            return (ExpLiteral (Decimal n))
        TokParenIzq -> do
            advance
            expr <- expresion
            consume TokParenDer "Se esperaba ')' después de la expresión"    
            return expr -- No necesitamos ExpAgrupada, los parentesis ya están implicitos
        TokOperador "-" -> do
            advance
            expr <- factor
            return (ExpUnaria "-" expr)
        _ -> Parser $ \_ -> Left (ErrorToken "Se esperaba un número, '(' o '-'")
            
-- Función Auxiliar para parsear desde string (require lexer)
parserFromString :: String -> ParseResult Expresion
parseFromString input =
    case tokenize input of
        Left err -> Left (ErrorSintaxis $ "Error de lexer: " ++ err)  
        Right tokens -> parseExpression tokens
  where
  -- Placeholder para el lexer - esto se implementará en el módulo Lexer
  tokenize :: String -> Either String [Token]
  tokenize _ = Left "Lexer no implementado aún"
    
-- Función auxiliar para mostrar errores de manera amigable
showParseError err = case err of
  ErrorToken msg -> "Error de sintaxis: " ++ msg
  ErrorEOF msg -> "Error: fin inesperado de la expresión - " ++ msg
  ErrorSintaxis msg -> "Error de sintaxis: " ++ msg
