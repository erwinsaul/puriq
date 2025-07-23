module Puriq.Lexer 
   ( tokenizar
   )where

import Puriq.Types
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

-- Funcion principal del lexer 
tokenizar :: String -> [Token]
tokenizar "" = [TokFin]
tokenizar entrada = tokenizarLista entrada ++ [TokFin]

-- Funcioón placeholder para el lexer
tokenizarLista :: String -> [Token]
tokenizarLista "" = []
tokenizarLista (c:cs) 
                      -- Ignorar Espacios en blanco
                      | isSpace c = tokenizarLista cs

                      -- Numeros(enteros y decimales)
                      | isDigit c = 
                            let (numeroStr, resto) = leerNumero (c:cs)
                            in crearTokenNumero numeroStr : tokenizarLista resto 

                      -- Operadores aritméticos
                      | c =='+' = TokOperador "+" : tokenizarLista cs
                      | c =='-' = TokOperador "-" : tokenizarLista cs
                      | c =='*' = TokOperador "*" : tokenizarLista cs
                      | c =='/' = TokOperador "/" : tokenizarLista cs
                      -- Parentesis
                      | c =='(' = TokParenIzq : tokenizarLista cs
                      | c == ')' = TokParenDer : tokenizarLista cs

                      -- Identificadores (para variables futuras)
                      | isAlpha c = 
                          let (identificador, resto) = leerIdentificador (c:cs)
                          in TokIdentificador identificador : tokenizarLista resto

                      -- Caracteres no reconocidos (error)
                      | otherwise = []

-- Funcion para leer números (enteros y decimales)
leerNumero :: String -> (String, String)
leerNumero entrada = leerNumero' "" entrada False
  where
    leerNumero' acc "" _ = (acc, "")
    leerNumero' acc (c:cs) puntoVisto
                                     | isDigit c = leerNumero' (acc ++ [c]) cs puntoVisto
                                     | c == '.' && not puntoVisto = leerNumero' (acc ++ [c]) cs True
                                     | otherwise = (acc, c:cs)

-- Función para crear token de número (entero o decimal)
crearTokenNumero :: String -> Token
crearTokenNumero numeroStr
                          | '.' `elem` numeroStr = TokDecimal (read numeroStr)
                          | otherwise = TokEntero (read numeroStr)

 -- Funcion para leer identificadores
leerIdentificador :: String -> (String, String)
leerIdentificador entrada = leerIdentificador' "" entrada
  where
     leerIdentificador' acc "" = (acc, "")
     leerIdentificador' acc (c:cs)
                                   | isAlphaNum c || c == '-' = leerIdentificador' (acc ++ [c]) cs
                                   | otherwise = (acc, c:cs)

-- Funcion de prueba para el REPL
probarLexer :: String -> IO ()
probarLexer entrada = do
    putStrLn $ "Entrada: " ++ entrada
    putStrLn $ "Tokens: " ++ show (tokenizar entrada)
    putStrLn ""
                      
