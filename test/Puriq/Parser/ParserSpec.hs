module Puriq.Parser.ParserSpec where

import Test.Hspec
import Puriq.Types
import Puriq.Parser

--Helper para crear tokens fácilmente
entero :: Int -> Expresion
entero n = ExpLiteral (Entero n)

decimal :: Double -> Expresion
decimal n = ExpLiteral (Decimal n)

-- Helper para operaciones binarias
binaria :: String -> Expresion -> Expresion -> Expresion
binaria = ExpBinaria

-- Test del parser
spec :: Spec
spec = do
  describe "Parser de expresiones artiméticas" $ do

    describe "Números simples" $ do
      it "parsea número entero" $ do
        let tokens = [TokEntero 42, TokFin]
        parseExpression tokens `shouldBe` Right (entero 42)

      it "parsea número decimal" $ do
        let tokens = [TokDecimal 3.14, TokFin]
        parseExpression tokens `shouldBe` Right (decimal 3.14)

    describe "Operaciones binarias" $ do
      it "parsea suma simple" $ do
        let tokens = [TokEntero 2, TokOperador "+", TokEntero 3, TokFin]
        parseExpression tokens `shouldBe` Right (binaria "+" (entero 2) (entero 3))

      it "parsea resta simple" $ do
        let tokens = [TokEntero 5, TokOperador "-", TokEntero 2, TokFin]
        parseExpression tokens `shouldBe` Right (binaria "-" (entero 5) (entero 2))
      it "parsea multiplicación simple" $ do
        let tokens = [TokEntero 4, TokOperador "*", TokEntero 6, TokFin]
        parseExpression tokens `shouldBe` Right (binaria "*" (entero 4) (entero 6))
        
      it "parsea división simple" $ do
        let tokens = [TokEntero 8, TokOperador "/", TokEntero 2, TokFin]
        parseExpression tokens `shouldBe` Right (binaria "/" (entero 8) (entero 2) )
    describe "Operaciones con números decimales" $do
      it "suma con decimales" $ do
        let tokens = [TokDecimal 2.5, TokOperador "+", TokDecimal 1.3, TokFin]
        parseExpression tokens `shouldBe` Right (binaria "+" (decimal 2.5) (decimal 1.3))

      it "mezcla enteros y decimales" $ do
        let tokens = [TokDecimal 2.5, TokOperador "+", TokDecimal 1.3, TokFin]
        parseExpression tokens `shouldBe` Right (binaria "+" (decimal 2.5) (decimal 1.3))
    
    describe "Precedencia de operadores" $ do
      it "multiplicación tiene mayor precedencia que suma" $ do
        let tokens = [TokEntero 2, TokOperador "+" , TokEntero 3, TokOperador "*", TokEntero 4, TokFin]
        let expected = binaria "+" (entero 2) (binaria "*" (entero 3) (entero 4))
        parseExpression tokens `shouldBe` Right expected

      it "divisón tiene mayor precedencia que resta" $ do
        let tokens = [TokEntero 10, TokOperador "-", TokEntero 6, TokOperador "/", TokEntero 2, TokFin]
        let expected = binaria "-" (entero 10) (binaria "/" (entero 6) (entero 2))
        parseExpression tokens `shouldBe` Right expected

      it "operadores del mimos nivel asociativos por la izquierda" $ do
        let tokens = [TokEntero 10, TokOperador "-", TokEntero 3, TokOperador "-", TokEntero 2, TokEntero 2, TokFin]
        let expected = binaria "-" (binaria "-" (entero 10) (entero 3)) (entero 2)
        parseExpression tokens `shouldBe` Right expected

    describe "Paréntesis" $ do
      it "parse expresión simple entre paréntesis" $ do
        let tokens = [TokParenIzq, TokEntero 5, TokParenDer, TokFin]
        parseExpression tokens `shouldBe` Right (entero 5)

      it "paréntesis cambian precendencia" $ do
        let tokens = [TokParenIzq, TokEntero 2, TokOperador "+", TokEntero 3, TokParenDer, TokOperador "*", TokEntero 4, TokFin]
        let expected = binaria "*" (binaria "+" (entero 2) (entero 3)) (entero 4)
        parseExpression tokens `shouldBe` Right expected

      it "maneja paréntesis anidados" $ do
        let tokens = [TokParenIzq, TokParenIzq, TokEntero 1, TokOperador "+", TokEntero 2, TokParenDer, TokOperador "*", TokEntero 3, TokParenDer, TokFin]
        let inner = binaria "+" (entero 1) (entero 2)
        let expected = binaria "*" inner (entero 3)
        parseExpression tokens `shouldBe` Right expected

    describe "Operadores unarios" $ do
      it "parsea número negativo" $ do
        let tokens = [TokOperador "-", TokEntero 5, TokFin]
        parseExpression tokens `shouldBe` Right (ExpUnaria "-" (entero 5))

      it "maneja negación en expresiones complejas" $ do
        let tokens = [TokOperador "-", TokEntero 2, TokOperador "+", TokEntero 3, TokFin]
        let expected = binaria "+" (ExpUnaria "-" (entero 2)) (entero 3)
        parseExpression tokens `shouldBe` Right expected

    describe "Errores de sintaxis" $ do
      it "detecta operador sin operando derecho" $ do
        let tokens = [TokEntero 2, TokOperador "+", TokFin]
        case parseExpression tokens of
          Left (ErrorToken _) -> True `shouldBe` True
          _ -> expectationFailure "Se esperaba ErrorToken"

      it "detecta paréntesis sin cerrar" $ do
        let tokens = [TokParenIzq, TokEntero 5, TokFin]
        case parseExpression tokens of
          Left (ErrorToken _) -> True `shouldBe` True
          _ -> expectationFailure "Se esperaba ErrorToken"

      it "detecta operador al inicio sin negación" $ do
        let tokens = [TokOperador "+", TokEntero 5, TokFin]
        case parseExpression tokens of
          Left (ErrorToken _) -> True `shouldBe` True
          _ -> expectationFailure "Se esperaba ErrorToken"

    describe "Expresiones complejas" $ do
      it "parse expresión aritmética compleja" $ do
        -- (2+3) * 4 - 5 / 2
        let tokens = [ TokParenIzq, TokEntero 2, TokOperador "+", TokEntero 3, TokParenDer, TokOperador "*", TokEntero 4, TokOperador "-", TokEntero 5, TokOperador "/", TokEntero 2, TokFin]
        let suma = binaria "+" (entero 2) (entero 3)
        let mult = binaria "*" suma (entero 4)
        let div' = binaria "/" (entero 5) (entero 2)
        let expected = binaria "-" mult div'
        parseExpression tokens `shouldBe` Right expected 

      it "expresión con muchos niveles de precendencia" $ do
        -- -2 * 3 + 4 / 2 - 1
        let tokens = [ TokOperador "-", TokEntero 2, TokOperador "*", TokEntero 3,
                       TokOperador "+", TokEntero 4, TokOperador "/", TokEntero 2,
                       TokOperador "-", TokEntero 1, TokFin ]

        let neg2 = ExpUnaria "-" (entero 2)
        let mult = binaria "*" neg2 (entero 3)
        let div' = binaria "/" (entero 4) (entero 2)
        let suma = binaria "+" mult div'
        let expected = binaria "-" suma (entero 1)
        parseExpression tokens `shouldBe` Right expected                
