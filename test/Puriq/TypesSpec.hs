module Puriq.TypesSpec (spec) where


import Test.Hspec
import Test.QuickCheck
import Puriq.Types
spec :: Spec
spec = do
  describe "Valor" $ do
    describe "Equality" $ do
      it "dos enteros iguales son iguales" $ do
        Entero 42 `shouldBe` Entero 42

      it "dos enteros diferentes no son iguales" $ do
        Entero 42 `shouldNotBe` Entero 24

      it "entero y decimal con mismo valor no son iguales" do
        Entero 42 `shouldNotBe` Decimal 42.0

    describe "Show instance" $ do
      it "muestra enteros correctamente" $ do
        show (Entero 42) `shouldBe` "Entero 42"

      it "muestra decimales correctamente" $ do
        show (Decimal 3.14) `shouldBe` "Decimal 3.14"

      it "muestra cadenas correctamente" $ do
        show (Cadena "hola") `shouldBe` "Cadena \"hola\""

      it "muestra booleanos correctamente" $ do
        show (Booleano True) `shouldBe` "Booleano True"
        show (Booleano False) `shouldBe` "Booleano False"

      it "muestra nulo correctamente" $ do
        show Nulo `shouldBe` "Nulo"


  describe "Token" $ do
    describe "Show instance" $ do
      it "muestra tokens de enteros" $ do
        show (TokEntero 42) `shouldBe` "TokEntero 42"

      it "muestra tokens de identificadores" $ do
        show (TokIdentificador "variable") `shouldBe` "TokIdentificador \"variable\""

  describe "Expresion" $ do
    describe "Show instance" $ do
      it "muestra expresiones literales" $ do
        show (ExpLiteral (Entero 42)) `shouldBe` "ExpLiteral (Entero 42)"

      it "muestra expresiones de variables" $ do
        show (ExpVariable "x") `shouldBe` "ExpVariable \"x\""

instance Arbitrary Valor where
  arbitrary = oneof
    [Entero <$> arbitrary,
     Decimal <$> arbitrary,
     Cadena <$> arbitrary,
     Booleano <$> arbitrary,
     pure Nulo    	
    ]  

prop_showRead :: Valor -> Bool
prop_showRead v = length (show v) > 0

spec_properties :: Spec
spec_properties = do
  describe "QuickCheck Propiertes" $ do
    it "show produce output no vacio" $ do
      property prop_showRead
           
      
