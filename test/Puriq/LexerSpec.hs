module Puriq.LexerSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Puriq.Types
import Puriq.Lexer

spec::Spec
spec = do
  describe "tokenizar" $ do
    describe "casos básicos" $ do
      it "tokenizar números simples" $ do
        tokenizar "42" `shouldBe` [TokEntero 42, TokFin]

      it "maneja entrada vacía" $ do
        tokenizar "" `shouldBe` [TokFin]

      -- Estos test fallarán inicialmente hasta implementar el lexer

      pending "tokenizar operadors básicos" $ do
        tokenizar "+" `shouldBe` [TokOperador "+", TokFin]
      
      pending "tokenizar expresiones simples" $ do
        tokenizar "2 + 3" `shouldBe` [TokEntero 2, TokOperador "+", TokEntero 3, TokFin]

    describe "casos edge" $ do
      pending "maneja espacios blancos" $ do
        tokenizar " 42 " `shouldBe` [TokEntero 42, TokFin]

      pending "maneja múltiples espacios" $ do
        tokenizar "2  +  3" `shouldBe` [TokEntero 2, TokOperador "+", TokFin]
   
        
