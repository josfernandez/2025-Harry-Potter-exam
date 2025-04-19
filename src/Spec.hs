module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()

correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

-- Test 2) ESTADO DE SALUD

-- Test a. En buen estado
  describe "Tests en buen estado:" $ do
    it "Auto marca peugeot no esta en buen estado" $ do
      enBuenEstado peugeot `shouldBe` False

    it "Auto Lamborghini con 99s y chasis 7 está en buen estado" $
      enBuenEstado (Auto "Lamborghini" "" (Desgaste 7 0) 0 99 []) `shouldBe` True

    it "Auto Fiat con 99s y desgaste de chasis 33 no está en buen estado" $
      enBuenEstado (Auto "Fiat" "" (Desgaste 33 0) 0 99 []) `shouldBe` False

    it "Auto Ferrari con 130s, ruedas 50 y chasis 30 está en buen estado" $
      enBuenEstado (Auto "Ferrari" "" (Desgaste 30 50) 0 130 []) `shouldBe` True

    it "Auto Ferrari con 15s, ruedas 50 y chasis 45 no está en buen estado" $
      enBuenEstado (Auto "Ferrari" "" (Desgaste 45 50) 0 15 []) `shouldBe` False

    it "Auto Ferrari con 150s, ruedas 70 y chasis 30 no está en buen estado" $
      enBuenEstado (Auto "Ferrari" "" (Desgaste 30 70) 0 150 []) `shouldBe` False

-- Test b. No da mas