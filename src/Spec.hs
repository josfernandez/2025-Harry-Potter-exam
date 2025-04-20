module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()

correrTests = hspec $ do
--  describe "Test de ejemplo" $ do
--  it "El pdepreludat se instaló correctamente" $ do
--      doble 1 `shouldBe` 2

-- Test 2) ESTADO DE SALUD

-- Test a. En buen estado
  describe "Tests para determinar el Buen Estado de un auto:" $ do
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
  describe "Tests para saber si un auto no da mas:" $ do
    it "Auto Marca Ferrari con desgaste de ruedas 20 y chasis 90" $ do
      noDaMas (Auto "Ferrari" "F50" (Desgaste 90 20) 5 0 ["La nave","El fierro","Ferrucho"]) `shouldBe` True

    it "Auto Marca Ferrari con desgaste de ruedas 90 y chasis 20" $ do
      noDaMas (Auto "Ferrari" "F50" (Desgaste 20 90) 5 0 ["La nave","El fierro","Ferrucho"]) `shouldBe` False

    it "Auto Marca Lamborghini con desgaste de ruedas 90 y chasis 20" $ do
      noDaMas (Auto "Lamborghini" "Diablo" (Desgaste 20 90) 73 0 ["Lambo","La bestia"]) `shouldBe` True

    it "Auto Marca Lamborghini" $ do
      noDaMas lamborghini `shouldBe` False

      -- Test c. Es un chiche
  describe "Tests para saber si el auto es un chiche:" $ do
    it "Un auto de marca Lamborghini" $ do
      esUnChiche lamborghini `shouldBe` True

    it "Un auto de marca Lamborghini con desgaste de ruedas 90 y chasis 20" $ do
      esUnChiche (Auto "lamborghini" "" (Desgaste 20 90) 0 100  ["Lambo","La bestia"]) `shouldBe` False

    it "Un auto de marca Ferrari con desgaste de ruedas 20 y chasis 90" $ do
      esUnChiche (Auto "Ferrari" "" (Desgaste 90 20) 0 100  ["La nave","El fierro","Ferrucho"]) `shouldBe` False

    it "Un auto de marca Ferrari" $ do
      esUnChiche ferrari `shouldBe` True

      -- Test d. Es una joya
  describe "Tests para saber si el auto es una joya:" $ do
    it "Un auto de marca Peugeot" $ do
      esUnaJoya  (Auto "Peugeot" "504" (Desgaste 0 0) 40 0 ["La Bocha"])  `shouldBe` True

    it "Un auto de marca Ferrari" $ do
      esUnaJoya ferrari `shouldBe` False

      -- Test e.Nivel de chetez
  describe "Tests para conocer nivel de chetez:" $ do
    it "Un auto de marca Ferrari" $ do
     nivelDeChetez ferrari `shouldBe` 180
    
       -- Test f. Supercalifragilisticaespialidosa
  describe "Tests para conocer capacidad supercalifragilisticaespialidosa:" $ do
    it "Un auto de marca Ferrari" $ do
     capacidadSuperCali ferrari `shouldBe` 7

       -- Test g. RiesgoAuto
  describe "Tests para conocer el riesgo de un auto:" $ do
    it "Un auto de marca Lamborghini" $ do
     riesgoDelAuto lamborghini `shouldBe` 29.2

    it "Un auto de marca Fiat" $ do
      riesgoDelAuto fiat `shouldBe` 237.6

  -- Test a. Reparar auto
    it "Reparar un auto de marca Fiat" $ do
      desgasteChasis (desgaste (repararAuto fiat)) `shouldBe` 4.95
      desgasteRueda (desgaste (repararAuto fiat)) `shouldBe` 0

    it "Reparar un auto de marca Ferrari" $ do
      desgasteChasis (desgaste (repararAuto ferrari)) `shouldBe` 0
      desgasteRueda (desgaste (repararAuto ferrari)) `shouldBe` 0  
 
  -- Test b. Aplicar penalidad
    it "Aplicar penalidad de 20s a un ferrari con tiempo 10s en pista" $ do
      tiempoCarrera (aplicarPenalidad 20 (Auto "Ferrari" "F50" (Desgaste 0 0) 5 10 ["La nave","El fierro","Ferrucho"])) `shouldBe` 30
     
    it "Aplicar penalidad de 0s a un ferrari con tiempo 10s en pista" $ do
        tiempoCarrera (aplicarPenalidad 0 (Auto "Ferrari" "F50" (Desgaste 0 0) 5 10 ["La nave","El fierro","Ferrucho"])) `shouldBe` 10 
 
  -- Test c. Ponerle Nitro
    it "Ponerle nitro a un auto (20% mas velocidad maxima)" $ do
       velocidadMaxima (ponerleNitro fiat) `shouldBe` 52.8

    it "Ponerle nitro a un auto (20% mas velocidad maxima)" $ do