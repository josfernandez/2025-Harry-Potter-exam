module Spec where
import PdePreludat
import Library
import Test.Hspec

ferrari2 :: Auto
ferrari2 = Auto "Ferrari" "F50" (Desgaste 0 0) 49 0 ["La nave","El fierro","Ferrucho"]

peugeot2 :: Auto
peugeot2 = Auto "Peugeot" "504" (Desgaste 0 0) 49 0 ["La Bocha","La bolita","El rey del desierto"] 

peugeot3 :: Auto
peugeot3 = Auto "Peugeot" "504" (Desgaste 0 0) 50 0 ["La Bocha","La bolita","El rey del desierto"] 

autos :: [Auto]
autos = [peugeot3, peugeot2, ferrari2]

-- TEST de nivel de Joyes
ferrariTiempo201 :: Auto
ferrariTiempo201 = Auto "Ferrari" "F50" (Desgaste 0 0) 49 201 ["La nave","El fierro","Ferrucho"]

ferrariTiempo200 :: Auto
ferrariTiempo200 = Auto "Ferrari" "F50" (Desgaste 0 0) 49 200 ["La nave","El fierro","Ferrucho"]

peugeotTiempo200 :: Auto
peugeotTiempo200 = Auto "Peugeot" "504" (Desgaste 0 0) 49 200 ["La Bocha","La bolita","El rey del desierto"] 

lamborghiniTiempo200 :: Auto
lamborghiniTiempo200 = Auto "Lamborghini" "Diablo" (Desgaste 7 4) 73 200 ["Lambo","La bestia"]
lamborghiniSinApodo = Auto "Lamborghini" "Diablo" (Desgaste 7 4) 73 200 []

--Conjunto de autos
ferraris :: [Auto]
ferraris = [ferrariTiempo201, ferrariTiempo200]

ferrariPeugeot :: [Auto]
ferrariPeugeot = [ferrariTiempo200, peugeotTiempo200]

ferrariLambo :: [Auto]
ferrariLambo = [lamborghiniTiempo200, ferrariTiempo200]


correrTests :: IO ()

correrTests = hspec $ do
--  describe "Test de ejemplo" $ do
--  it "El pdepreludat se instaló correctamente" $ do
--      doble 1 `shouldBe` 2

-- Test 2) ESTADO DE SALUD

-- Test a. En buen estado
  describe "Tests para determinar si una auto esta en buen estado:" $ do
    it "Auto marca peugeot no esta en buen estado" $ do
      enBuenEstado peugeot `shouldBe` False

    it "Auto con tiempo en pista < 100 y desgaste de chasis < 20 esta en buen estado" $ do
      enBuenEstado (Auto "Lamborghini" "" (Desgaste 7 0) 0 99 []) `shouldBe` True
     
    it "Auto chasis menor a 40 y ruedas menor a 60 en buen estado" $ do
      enBuenEstado (Auto "Fiat" "" (Desgaste 33 0) 0 99 []) `shouldBe` False
      enBuenEstado (Auto "Ferrari" "" (Desgaste 30 50) 0 130 []) `shouldBe` True
      enBuenEstado (Auto "Ferrari" "" (Desgaste 30 70) 0 150 []) `shouldBe` False
      enBuenEstado (Auto "Ferrari" "" (Desgaste 45 50) 0 15 []) `shouldBe` False
      
-- Test b. No da mas
  describe "Tests para saber si un auto no da mas:" $ do
    it "Auto con primer apodo la y desgaste chasis mayor a 80" $ do
      noDaMas (Auto "Ferrari" "F50" (Desgaste 90 20) 5 0 ["La nave","El fierro","Ferrucho"]) `shouldBe` True
      noDaMas (Auto "Ferrari" "F50" (Desgaste 20 65) 5 0 ["La nave","El fierro","Ferrucho"]) `shouldBe` False

    it "Auto con desgaste ruedas mayor a 80" $ do
      noDaMas (Auto "Lamborghini" "Diablo" (Desgaste 20 90) 73 0 ["Lambo","La bestia"]) `shouldBe` True
      noDaMas lamborghini `shouldBe` False
   
      -- Test c. Es un chiche
  describe "Tests para saber si el auto es un chiche:" $ do
    it "un auto desgaste chasis menor a 20 y par de apodos" $ do
      esUnChiche lamborghini `shouldBe` True
      esUnChiche (Auto "lamborghini" "" (Desgaste 20 90) 0 100  ["Lambo","La bestia"]) `shouldBe` False

    it "Un auto impar de apodos, desgaste chasis menor a 50" $ do
      esUnChiche (Auto "Ferrari" "" (Desgaste 90 20) 0 100  ["La nave","El fierro","Ferrucho"]) `shouldBe` False
      esUnChiche ferrari `shouldBe` True
     
      -- Test d. Es una joya
  describe "Tests para saber si el auto es una joya:" $ do
    it "Un auto de marca Peugeot" $ do
      esUnaJoya  (Auto "Peugeot" "504" (Desgaste 0 0) 40 0 ["La Bocha"])  `shouldBe` True
      esUnaJoya ferrari `shouldBe` False

      -- Test e.Nivel de chetez
  describe "Tests para conocer nivel de chetez:" $ do
    it "Un auto con x cantidad de apodos e y cantidad de letras" $ do
     nivelDeChetez ferrari `shouldBe` 180
     
       -- Test f. Supercalifragilisticaespialidosa
  describe "Tests para conocer capacidad supercalifragilisticaespialidosa:" $ do
    it "Un auto de con 7 caracteres en su primer apodo" $ do
     capacidadSuperCali ferrari `shouldBe` 7

       -- Test g. RiesgoAuto
  describe "Tests para conocer el riesgo de un auto:" $ do
    it "un auto en buen estado (vel max*1/10*desgaste ruedas)" $ do
     riesgoDelAuto lamborghini `shouldBe` 29.2

    it "Un auto en mal estado (vel max*1/10*desgaste ruedas*2)" $ do
      riesgoDelAuto fiat `shouldBe` 237.6


--TESt 3) Manos a la obra

  -- Test a. Reparar auto
    it "Reparar un auto ,85% menos en desgaste chasis, y 0 desgaste ruedas" $ do
      desgasteChasis (desgaste (repararAuto fiat)) `shouldBe` 4.95
      desgasteRueda (desgaste (repararAuto fiat)) `shouldBe` 0
      desgasteChasis (desgaste (repararAuto ferrari)) `shouldBe` 0
      desgasteRueda (desgaste (repararAuto ferrari)) `shouldBe` 0  
    
  -- Test b. Aplicar penalidad
    it "Aplicar penalidad de x segundos, aumenta el tiempo en pista" $ do
      tiempoCarrera (aplicarPenalidad 20 (Auto "Ferrari" "F50" (Desgaste 0 0) 5 10 ["La nave","El fierro","Ferrucho"])) `shouldBe` 30
      tiempoCarrera (aplicarPenalidad 0 (Auto "Ferrari" "F50" (Desgaste 0 0) 5 10 ["La nave","El fierro","Ferrucho"])) `shouldBe` 10 
        
  -- Test c. Ponerle Nitro
    it "Ponerle nitro a un auto (20% mas velocidad maxima)" $ do
       velocidadMaxima (ponerleNitro fiat) `shouldBe` 52.8
       velocidadMaxima (ponerleNitro (Auto "Fiat" "600" (Desgaste 33 27) 0 0 ["La Bocha","La bolita","Fitito"])) `shouldBe` 0
        
  -- Test d. Bautizar un auto
  describe "Tests para bautizar un auto (agregar un apodo mas al auto):" $ do
    it "Bautizar 'El diablo' a un auto marca Lamborghini" $ do
      (apodo . bautizarElAuto "El diablo") lamborghini `shouldContain` ["El diablo"]
      (apodo . bautizarElAuto "El diablo") lamborghiniSinApodo `shouldContain` ["El diablo"]
{-
    --it "Bautizar 'El diablo' a un auto marca Lamborghini sin apodos" $ do
      --

  -- Test e. Llevar un auto a un desarmadero
  describe "Tests de desarmader:" $ do
    it "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca 'Tesla' modelo 'X'" $ do 
     (marca . llevarAutoADesarmadero "Tesla" "X") fiat `shouldBe` "Tesla"

    it "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca 'Tesla' modelo 'X'" $ do 
     (modelo . llevarAutoADesarmadero "Tesla" "X") fiat `shouldBe` "X"
    
    it "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca 'Tesla' modelo 'X'" $ do 
     (apodo . llevarAutoADesarmadero "Tesla" "X") fiat `shouldBe` ["Nunca Taxi"]


-- Test 4) Pistas

  --Test a
  describe "Tests de curvas peligrosas:" $ do
    it "Transitar una curva peligrosa  con un auto marca Ferrari" $ do
      (desgasteRueda . desgaste . desgasteDelAutoEnCurva ferrari) curvaPeligrosa `shouldBe` 15

    it "Transitar una curva peligrosa  con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnCurva ferrari) curvaPeligrosa `shouldBe` 0

    it "Transitar una curva peligrosa  con un auto marca peouget" $ do
      tiempoDelAutoEncurva peugeot curvaPeligrosa `shouldBe` 15

    it "Transitar una curva tranca  con un auto marca Ferrari" $ do
      (desgasteRueda . desgaste . desgasteDelAutoEnCurva ferrari) curvaTranca `shouldBe` 15

    it "Transitar una curva tranca  con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnCurva ferrari) curvaTranca `shouldBe` 0
    
    it "Transitar una curva tranca  con un auto marca peouget" $ do
      tiempoDelAutoEncurva peugeot curvaTranca `shouldBe` 27.5

  --Test b
  describe "Tests de rectas:" $ do
    it "Transitar un RetroClassic  con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnRecto ferrari) tramoRectoClassic `shouldBe` 7.15
    
    it "Transitar una RetroClassic con un auto marca Ferrari" $ do
      tiempoDelAutoEnRecto ferrari tramoRectoClassic `shouldBe` 11

    it "Transitar un Tramito con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnRecto ferrari) tramito `shouldBe` 2.6
    
    it "Transitar una Tramito con un auto marca Ferrari" $ do
      tiempoDelAutoEnRecto ferrari tramito `shouldBe` 4

  --Test c
  describe "Tests de zigZagLoco :" $ do
    it "Desgaste del chasis en un zigZagLoco con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnZigZag ferrari) zigZagLoco `shouldBe` 5
    
    it "Desgaste del ruedas en  zigZagLoco con un auto marca Ferrari" $ do
      (desgasteRueda . desgaste . desgasteDelAutoEnZigZag ferrari) zigZagLoco `shouldBe` 32.5
    
    it "El tiempo en pista en zigZagLoco con un auto marca Ferrari" $ do
      tiempoDelAutoEnZigZag ferrari zigZagLoco `shouldBe` 15
    
    it "Desgaste del chasis en un casiCurva con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnZigZag ferrari) casiCurva `shouldBe` 5
    
    it "Desgaste de ruedas en  casiCurva con un auto marca Ferrari" $ do
      (desgasteRueda . desgaste . desgasteDelAutoEnZigZag ferrari) casiCurva `shouldBe` 6.5
    
    it "El tiempo en pista en casiCurva con un auto marca Ferrari" $ do
      tiempoDelAutoEnZigZag ferrari casiCurva `shouldBe` 3
  
  --Test d    
  describe "Tests de Rulo :" $ do
    it "Desgaste del chasis en un ruloClasico con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnRulo ferrari) ruloClasico `shouldBe` 0
    
    it "Desgaste de ruedas en  ruloClasico con un auto marca Ferrari" $ do
      (desgasteRueda . desgaste . desgasteDelAutoEnRulo ferrari) ruloClasico `shouldBe` 19.5
    
    it "El tiempo en pista en ruloClasico con un auto marca Ferrari" $ do
      tiempoDelAutoEnRulo ferrari ruloClasico `shouldBe` 1
    
    it "Desgaste del chasis en un deseoDeMuerte con un auto marca Ferrari" $ do
      (desgasteChasis . desgaste . desgasteDelAutoEnRulo ferrari) deseoDeMuerte `shouldBe` 0
    
    it "Desgaste de ruedas en  deseoDeMuerte con un auto marca Ferrari" $ do
      (desgasteRueda . desgaste . desgasteDelAutoEnRulo ferrari) deseoDeMuerte `shouldBe` 39
    
    it "El tiempo en pista en deseoDeMuerte con un auto marca Ferrari" $ do
      tiempoDelAutoEnRulo ferrari deseoDeMuerte `shouldBe` 2
-}
-- Test 5) Nivel de Joyez

  describe "Tests de nivel de joyez :" $ do
    it "Probamos el nivel de joyez de 3 autos" $ do
      nivelDeJoyez autos `shouldBe` 3

  describe "Tests de paraEntendidos :" $ do
    it "Probamos el para entendidos con ferraris " $ do
      paraEntendidos ferraris `shouldBe` False

    it "Probamos el para entendidos con ferrari y peugeot" $ do
      paraEntendidos ferrariPeugeot `shouldBe` False

    it "Probamos el para entendidos en ferrari y lamborghini " $ do
      paraEntendidos ferrariLambo `shouldBe` True
    

  