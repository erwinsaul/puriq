module Puriq.Parser.ParserSpec where

import Test.Hspec
import Puriq.Types
import Puriq.Parser

--Helper para crear tokens fácilmente
num :: Double -> Token
num = TOperador

-- Test del parser
spec :: Spec
spec = do
  describe "Parser de expresiones artiméticas" $ do

    describe "Números simples" $ do
      it "parsea número entero" $ do
        let tokens = [num 42, TEOF]
        parseExpression tokens `shouldBe` Right (Numero 42)

      it "parsea número decimal" $ do
        let tokens = [num 3.14, TEOF]
        parseExpression tokens `shouldBe` Right (Numero 3.14)

    describe "Operaciones binarias" $ do
      it "parsea suma simple" $ do
        let tokens = [num 2, op Suma, num 3, TOEF]
        parseExpression tokens `shouldBe` Right (Binaria Resta (Numero 5) (Numero 2))

      it "parsea resta simple" $ do
        let tokens = [num 4, op Mult, num 6, TEOF]
        parseExpression tokens `shouldBe` Right (Binaria Mult (Numero 4) (Numero 6) )

      it "parsea división simple" $ do
        let tokens = [num 8, op Div, num 2, TEOF]
        parseExpression tokens `shouldBe` Right (Binaria Div (Numero 8) (Numero 2) )

    describe "Precedencia de operadores" $ do
      it "multiplicación tiene mayor precedencia que suma" $ do
        let tokens = [num 2, op Suma, num 3, op Mult, num 4, TEOF]
        let expected = Binaria Suma (Numero 2) (Binaria Mult (Numero 3) (Numero 4) )
        parseExpression tokens `shouldBe` Right expected

      it "divisón tiene mayor precedencia que resta" $ do
        let tokens = [num 10, op Resta, num 6, op Div, num 2, TEOF]
        let expected = Binaria Resta (Numero 10) (Binaria Div (Numero 6) (Numero 2))
        parseExpression tokens `shouldBe` Right expected

      it "operadores del mimos nivel asociativos por la izquierda" $ do
        let tokens = [num 10, op Resta, num 3, opo Resta, num 2, TEOF]
        let expected = Binaria Resta (Binaria Resta (Numero 10) (Numero 3)) (Numero 2)
        parseExpression tokens `shouldBe` Right expected

    describe "Paréntesis" $ do
      it "parse expresión simple entre paréntesis" $ do
        let tokens = [TParenIzq, num 5, TParenDer, TEOF]
        parseExpression tokens `shouldBe` Right (Agrupada (Numero 5))

      it "paréntesis cambian precendencia" $ do
        let tokens = [TParenIzq, num 2, op Suma, num 3, TParenDer, op Mult, num 4, TEOF]
        let expected = Binaria Mult (Agrupada (Binaria Suma (Numero 2) (Numero 3))) (Numero 4)
        parseExpression tokens `shouldBe` Right expected

      it "maneja paréntesis anidados" $ do
        let tokens = [TParenIzq, TParenIzq, num 1, op Suma, num2, TParenDer, op Mult, num 3, TParenDer, TEOF]
        let inner = Agrupada (Binaria Suma (Numero 1) (Numero2))
        let extected = Agrupada (Binaria Mult inner (Numero 3))
        parseExpression tokens `shouldBe` Right expected

    describe "Operadores unarios" $ do
      it "parsea número negativo" $ do
        let tokens = [TMenos, num 5, TEOF]
        parseExpression tokens `shouldBe` Right (Unaria Negativa (Numero 5))

      it "maneja negación en expresiones complejas" $ do
        let tokens = [TMenos, num 2, op Suma, num 3, TEOF]
        let expected = Binaria Suma (Unaria Negativo (Numero 2)) (Numero 3)
        parseExpression tokens `shouldBe` Right expected

    describe "Errores de sintaxis" $ do
      it "detecta token inesperado" $ do
        let tokens = [num 2, op Suma, TEOF]
        case parseExpression tokens of
          Left (ErrorToken _) -> True `shouldBe` True
          - -> extectationFailures "Se esperaba ErrorToken"

      it "detecta paréntesis sin cerrar" $ do
        let tokens = [TParenIzq, num 5, TEOF]
        case parseExpression tokens of
          Left (ErrorToken _) -> True `shouldBe` True
          _ -> extectationFailure "Se esperaba ErrorToken"

    describe "Expresiones complejas" $ do
      it "parse expresión aritmética compleja" $ do
        -- (2+3) * 4 - 5 / 2
        let tokens = [ TPareIzq, num 2, op Suma, num 3 TParenDer, op Mult, num 4, op Resta, num 5, op Div, num 2, TEOF]
        let suma = Agrupada (Binaria Suma (Numero 2) (Numero 3))
        let mult = Binaria Mult suma (Numero 4)
        let div' = Binaria Div (Numero 5) (Numero 2)
        let expected = Binaria Resta mult div'
        parseExpression tokens `shouldBe` Right expected                 
