module Puriq.Parser.Utils where

import Puriq.Types

-- Pretty printing del AST para debugging
prettyPrint :: Expresion -> String
prettyPrint expr = prettyPrint' expr 0
  where
    prettyPrint' :: Expresion -> Int -> String
    prettyPrint' e depth =
      let indent = replicate (depth*2) ' '
      in case e of
        ExpLiteral valor -> indent ++ "Literal " ++ show valor
        ExpVariable nombre -> indent ++ "Variable " ++ nombre
        ExpBinaria op left right ->
          indent ++ "Binaria " ++ show op ++ "\n" ++
          prettyPrint' left (depth + 1) ++ "\n" ++
          prettyPrint' right (depth + 1)
        ExpUnaria op expr ->
          indent ++ "Unaria " ++ show op ++ "\n" ++
          prettyPrint' expr (depth + 1)

-- Convertir AST a notacion infija (para mostrar al usuario)
toInfix :: Expresion -> String
toInfix expr = case expr of
  ExpLiteral (Entero n) -> show n
  ExpLiteral (Decimal n) -> show n
  ExpLiteral (Cadena s) -> "\"" ++ s ++ "\""
  ExpLiteral (Booleano True) -> "verdadero"
  ExpLiteral (Booleano False) -> "falso"
  ExpLiteral Nulo -> "nulo"
  ExpVariable nombre -> nombre
  ExpBinaria op left right ->
    "(" ++ toInfix left ++ " " ++ op ++ " " ++ toInfix right ++ ")"
  ExpUnaria op expr ->
    op ++ toInfixWithParens expr
  where
  	-- Añadir paréntesis solo cuando sea necesario
  	toInfixWithParens :: Expresion -> String
  	toInfixWithParens e@(ExpBinaria _ _ _) = "(" ++ toInfix e ++ ")"
  	toInfixWithParens e = toInfix e

 -- Convertir AST a notación infija sin paréntesis innecesarios (más limpio)
toInfixClean :: Expresion -> String
toInfixClean expr = toInfixClean' expr 0
   where
     toInfixClean' :: Expresion -> Int -> String
     toInfixClean' e parentPrec = case e of
       ExpLiteral (Entero n) -> show n
       ExpLiteral (Decimal n) -> show n
       ExpLiteral (Cadena s) -> "\"" ++ s ++ "\""
       ExpLiteral (Booleano True) -> "verdadero"
       ExpLiteral (Booleano False) -> "falso"
       ExpLiteral Nulo -> "nulo"
       ExpVariable nombre -> nombre
       ExpBinaria op left right ->
         let prec = precedencia op
             result = toInfixClean' left prec ++ " " ++ op ++ " " ++ toInfixClean' right prec
         in if prec < parentPrec then "(" ++ result ++ ")" else result
       ExpUnaria op expr ->
         op ++ toInfixClean' expr 10 -- Precedencia alta para unarios

     -- Precedencia de operadores (mayor número = mayor precedencia)
     precedencia :: String -> Int
     precedencia "*" = 6
     precedencia "/" = 6
     precedencia "+" = 5
     precedencia "-" = 5
     precedencia _ = 0

-- validar que un AST sea bien formado (útil para debugging)
validateAST :: Expresion -> Bool
validateAST expr = case expr of
  ExpLiteral _ -> True
  ExpVariable _ -> True
  ExpBinaria _ left right -> validateAST left && validateAST right
  ExpUnaria _ expr -> validateAST expr

-- Contar operadores en una expresión (Útil para estadísticas)
countOperators :: Expresion -> Int
countOperators expr = case expr of
  ExpLiteral _ -> 0
  ExpVariable _ -> 0
  ExpBinaria _ left right -> 1 + countOperators left + countOperators right
  ExpUnaria _ expr -> 1 + countOperators expr

-- Obtener la profundidad del arbol
getDepth :: Expresion -> Int
getDepth expr = case expr of
  ExpLiteral _ -> 1
  ExpVariable _ -> 1
  ExpBinaria _ left right -> 1 + max (getDepth left) (getDepth right)
  ExpUnaria _ expr -> 1 + getDepth expr

-- Extraer todos los valores literales de una expresion
extractLiterals :: Expresion -> [Valor]
extractLiterals expr = case expr of
  ExpLiteral valor -> [valor]
  ExpVariable _ -> []
  ExpBinaria _ left right -> extractLiterals left ++ extractLiterals right
  ExpUnaria _ expr -> extractLiterals expr

-- Extraer todas las variables de una expresion

extractVariables :: Expresion -> [String]
extractVariables expr = case expr of
  ExpLiteral _ -> []
  ExpVariable nombre -> [nombre]
  ExpBinaria _ left right -> extractVariables left ++ extractVariables right
  ExpUnaria _ expr -> extractVariables expr

-- Función para testing: crear una expresión binaria simple
makeBinary :: String -> Int -> Int -> Expresion
makeBinary op left right = ExpBinaria op (ExpLiteral (Entero left)) (ExpLiteral (Entero right))
          
-- Funcion para testing: crear una expresión unaria simple
makeUnary :: String -> Int -> Expresion
makeUnary op value = ExpUnaria op (ExpLiteral (Entero value))

-- Verificar si una expresión es aritmética (solo contiene números y operdores)
isArithmetic :: Expresion -> Bool
isArithmetic expr = case expr of
  ExpLiteral (Entero _) -> True
  ExpLiteral (Decimal _) -> True
  ExpLiteral _ -> False
  ExpVariable _ -> False
  ExpBinaria op left right ->
    op `elem` ["+", "-", "*", "/"] && isArithmetic left && isArithmetic right
  ExpUnaria op expr ->
    op == "-" && isArithmetic expr

-- Contar tipos específicos de tokens en una expresión
countNumbers :: Expresion -> Int
countNumbers expr = case expr of
  ExpLiteral (Entero _) -> 1
  ExpLiteral (Decimal _) -> 1
  ExpLiteral _ -> 0
  ExpVariable _ -> 0
  ExpBinaria _ left right -> countNumbers left + countNumbers right
  ExpUnaria _ expr -> countNumbers expr

-- Función auxiliar para mostrar errores en español
showError :: String -> String
showError msg = "Error: " ++ msg
