module Puriq.ReplSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Puriq.Repl (procesarEntrada)
import Puriq.Types (Valor(..))

spec :: Spec
spec = do
    describe "procesarEntrada" $ do

        describe "entrada vacia" $ do
            it "retorna el mismo entorno sin cambios" $ do
                env <- procesarEntrada Map.empty ""
                env `shouldBe` Map.empty
        
        describe "expresiones aritméticas" $ do
            it "evalúa una expresión y retorna el mismo entorno (sin asignación)" $ do
                env <- procesarEntrada Map.empty "2 + 3"
                env `shouldBe` Map.empty
            
        describe "asignación de variables" $ do
            it "agrega la variable al entorno tras asignarla" $ do
                env <- procesarEntrada Map.empty "x = 5"
                Map.lookup "x" env `shouldBe` Just (Entero 5)
        
        describe "error de parsing" $ do
            it "retorna el entorno sin cambios ante entrada inválida" $ do
                env <- procesarEntrada Map.empty "x +"
                env `shouldBe` Map.empty