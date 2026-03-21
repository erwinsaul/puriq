module Puriq.EvaluatorSpec (spec) where

import Test.Hspec
import Puriq.Types
import Puriq.Evaluator
import Puriq.Parser (parseFromString)
import qualified Data.Map.Strict as Map

--import Test.QuickCheck
--import Puriq.Types

spec :: Spec
spec = do
  describe "Evaluator de expresiones matemáticas" $ do

    describe "literales" $ do
      it "evalúa un entero literal" $ do
        case parseFromString "42" of
          Left err -> expectationFailure $ "Error de parsing: "++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero 42, Map.empty)
      
      it "evalúa un decimal literal" $ do
        case parseFromString "3.14" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty  expr `shouldBe` Right (Decimal 3.14, Map.empty)
    
    describe "suma" $ do
      it "suma dos enteros" $ do
        case parseFromString "2 + 3" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero 5, Map.empty)
    
      it "suma dos decimales" $ do
        case parseFromString "2.5 + 3.7" of
          Left err -> expectationFailure $ "Error de Parsing: " ++ show err
          Right expr -> evaluar  Map.empty expr `shouldBe` Right (Decimal 6.2, Map.empty)
      
      it "suma entero y decimal" $ do
        case parseFromString "2 + 3.5" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Decimal 5.5, Map.empty)
    
    describe "resta" $ do
      it "resta dos enteros" $ do
        case parseFromString "10-3" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero 7, Map.empty)

      it "resta con resultado negativo" $ do
        case parseFromString "3-10" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero (-7), Map.empty)

    describe "multiplicacion" $ do
      it "multiplica dos enteros" $ do
        case parseFromString "2 * 3" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr ->evaluar Map.empty expr `shouldBe` Right (Entero 6, Map.empty)
      
      it "multiplica entero y decimal" $ do
        case parseFromString "2 * 3.5" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Decimal 7.0, Map.empty)
    
    describe "division" $ do
      it "divide dos enteros (retorna decimal)" $ do
        case parseFromString "10 / 2" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Decimal 5.0, Map.empty)
      
      it "divide con resultado decimal" $ do
        case parseFromString "7 / 2" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Decimal 3.5, Map.empty)

      it "detecta division por cero (entero)" $ do
        case parseFromString "5 / 0" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Left ErrorDivisionPorCero

      it "detecta división por cero (decimal)" $ do
        case parseFromString "5.5 / 0.0" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Left ErrorDivisionPorCero
    
    describe "operador unario" $ do
      it "niega un entero" $ do
        case parseFromString "-5" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero (-5), Map.empty)
        
      it "niega un decimal" $ do
        case parseFromString "-3.14" of
          Left err -> expectationFailure $ "Error de parsing" ++ show err
          Right expr -> evaluar  Map.empty expr `shouldBe` Right (Decimal (-3.14), Map.empty)
      
      it "doble negacion" $ do
        case parseFromString "--5" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero 5, Map.empty)

    describe "expresiones complejas" $ do
      it "respeta precedencia: 2 + 3 * 4 = 14" $ do
        case parseFromString "2 + 3 * 4" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero 14, Map.empty)
      
      it "respeta precedencia con division: 10 - 6 / 2 = 7" $ do
        case parseFromString "10 - 6 / 2" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Decimal 7.0, Map.empty)
      
      it "evalua expresion con parentesis: (2 + 3 * 4) = 20" $ do
        case parseFromString "(2 + 3) * 4" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty expr `shouldBe` Right (Entero 20, Map.empty)
      
      it "evalua expresion compleja: 2 + 3 * (4 - 1) = 11" $ do
        case parseFromString "2 + 3 * (4 - 1)" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar Map.empty  expr `shouldBe` Right (Entero 11, Map.empty)
