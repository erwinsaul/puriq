module Puriq.EvaluatorSpec (spec) where

import Test.Hspec
import Puriq.Types
import Puriq.Evaluator
import Puriq.Parser (parseFromString)
--import Test.QuickCheck
--import Puriq.Types

spec :: Spec
spec = do
  describe "Evaluator de expresiones matemáticas" $ do

    describe "literales" $ do
      it "evalúa un entero literal" $ do
        case parseFromString "42" of
          Left err -> expectationFailure $ "Error de parsing: "++ show err
          Right expr -> evaluar expr `shouldBe` Right (Entero 42)
      
      it "evalúa un decimal literal" $ do
        case parseFromString "3.14" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar expr `shouldBe` Right (Decimal 3.14)
    
    describe "suma" $ do
      it "suma dos enteros" do
        case parseFromString "2 + 3" of
          Left err -> expectationFailure $ "Error de parsing: " ++ show err
          Right expr -> evaluar expr `shouldBe` Right (Entero 5)
    
      
