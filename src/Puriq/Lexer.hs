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
                      -- Cadenas con comillas dobles
                      | c == '"' =
                        let (cadena, resto) = leerCadena '"' cs
                        in TokCadena cadena : tokenizarLista resto
                      -- Cadenas con comillas simples
                      | c == '\'' =
                          let (cadena, resto) = leerCadena '\'' cs
                          in TokCadena cadena : tokenizarLista resto
                      -- Identificadores (para variables futuras)
                      | isAlpha c || c == '_' = 
                          let (identificador, resto) = leerIdentificador (c:cs)
                          in clasificarPalabra identificador : tokenizarLista resto

                      -- Caracteres no reconocidos (error)
                      | otherwise = TokError c : tokenizarLista cs

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
                                   | isAlphaNum c || c == '_' = leerIdentificador' (acc ++ [c]) cs
                                   | otherwise = (acc, c:cs)

-- Funcion de prueba para el REPL
probarLexer :: String -> IO ()
probarLexer entrada = do
    putStrLn $ "Entrada: " ++ entrada
    putStrLn $ "Tokens: " ++ show (tokenizar entrada)
    putStrLn ""

-- Lee el contenido de una cadena hasta encontrar la comilla de cierre
leerCadena :: Char -> String -> (String, String)
leerCadena _ "" = ("", "")    -- cadena sin cerrar
leerCadena q (c:cs)
    | c == q   = ("", cs)
    | otherwise = let (resto, fin) = leerCadena q cs
                  in (c:resto, fin)

-- Clasifica si una palabra es keyword o identificador normal
clasificarPalabra :: String -> Token
clasificarPalabra "verdadero" = TokBooleano True
clasificarPalabra "falso" = TokBooleano False
clasificarPalabra "nulo" = TokPalabraReservada "nulo"
clasificarPalabra "si"   = TokPalabraReservada "si"
clasificarPalabra "sino" = TokPalabraReservada "sino"
clasificarPalabra "sino_si" = TokPalabraReservada "sino_si"
clasificarPalabra "mientras" = TokPalabraReservada "mientras"
clasificarPalabra "para" = TokPalabraReservada "para"
clasificarPalabra "en" = TokPalabraReservada "en"
clasificarPalabra "definir" = TokPalabraReservada "definir"
clasificarPalabra "retornar" = TokPalabraReservada "retornar"
clasificarPalabra "pasar" = TokPalabraReservada "pasar"
clasificarPalabra "romper" = TokPalabraReservada "romper"
clasificarPalabra "continuar" = TokPalabraReservada "continuar"
clasificarPalabra "clase" = TokPalabraReservada "clase"
clasificarPalabra "importar" = TokPalabraReservada "importar"
clasificarPalabra "es" = TokPalabraReservada "es"
clasificarPalabra "y" = TokPalabraReservada "y"
clasificarPalabra "o" = TokPalabraReservada "o"
clasificarPalabra "no" = TokPalabraReservada "no"
clasificarPalabra palabra = TokIdentificador palabra
                      
