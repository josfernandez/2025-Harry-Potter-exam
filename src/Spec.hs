module Spec where
import PdePreludat
import Library
import Test.Hspec

peugeotTiempo49 :: Auto
peugeotTiempo49 = Auto "Peugeot" "504" (Desgaste 0 0) 49 0 ["La Bocha","La bolita","El rey del desierto"] 

peugeotTiempo50 :: Auto
peugeotTiempo50 = Auto "Peugeot" "504" (Desgaste 0 0) 50 0 ["La Bocha","La bolita","El rey del desierto"] 

autos :: [Auto]
autos = [peugeotTiempo50, peugeotTiempo49, ferrari]

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

ferrari65k = Auto "Ferrari" "F50" (Desgaste 0 0) 65 0 ["La nave","El fierro","Ferrucho"]
fiat44k = Auto "Fiat" "" (Desgaste 33 0) 44 99 []
lambo73k = Auto "Lamborghini" "Diablo" (Desgaste 7 4) 73 200 ["Lambo","La bestia"]

ferraric10 = Auto "Ferrari" "F40" (Desgaste 10 0) 65 0 []
lamboc20 = Auto "Lambo" "sv" (Desgaste 20 0) 73 0 []
fiatc50 = Auto "Fiat" "argos" (Desgaste 50 0) 50 0 []


equipoConPresupuesto :: Equipo
equipoConPresupuesto = Equipo "ferrari lambo" [ferraric10, lamboc20] 20000

equipoConPresupuestoRecortado :: Equipo
equipoConPresupuestoRecortado = Equipo "ferrari lambo" [ferraric10, lamboc20] 10000

equipoPobre :: Equipo
equipoPobre = Equipo "fiat" [fiatc50] 10000

equipoItaloFrances :: Equipo
equipoItaloFrances = Equipo "Italo Frances" [fiatc50,peugeotTiempo49] 100000


equipoPeLa :: Equipo
equipoPeLa = Equipo "FERRARIÑO" [peugeotTiempo49,lamboc20] 20000

equipoFP :: Equipo
equipoFP = Equipo "FERRARI GUIÑO GUIÑO" [peugeotTiempo49,lamboc20] 4000

equipoPeLaFe :: Equipo
equipoPeLaFe = Equipo "Felali" [peugeotTiempo49,ferrari65k,lamboc20] 20000

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

  -- Test e. Llevar un auto a un desarmadero
  describe "Tests de desarmadero (cambia marca y modelo, y pierde apodos, nunca taxi):" $ do
    it "Llevar a un desarmadero a un auto marca Fiat para cambiar por marca 'Tesla' modelo 'X'" $ do 
     (marca . llevarAutoADesarmadero "Tesla" "X") fiat `shouldBe` "Tesla"
     (modelo . llevarAutoADesarmadero "Tesla" "X") fiat `shouldBe` "X"
     (apodo . llevarAutoADesarmadero "Tesla" "X") fiat `shouldBe` ["Nunca Taxi"]


-- Test 4) Pistas
  
  --Test a
  describe "Tests de curvas peligrosas:" $ do
    it "Desgaste de la rueda de un auto en una curva peligrosa" $ do
     (desgasteRueda . desgasteDelAutoEnCurvaPeligrosa) ferrari `shouldBe` 15
    it "Desgaste del chasis de un auto en una curva peligrosa" $ do
      (desgasteChasis . desgasteDelAutoEnCurvaPeligrosa) ferrari `shouldBe` 0
    it "Tiempo de pista de un auto en una curva peligrosa" $ do
      tiempoCarrera (curvaPeligrosa peugeot)  `shouldBe` 15

    it "Desgaste de la rueda de un auto en una curva tranca" $ do
      (desgasteRueda . desgasteDelAutoEnCurvaTranca) ferrari `shouldBe` 15
    it "Desgaste del chasis de un auto en una curva tranca" $ do
      (desgasteChasis . desgasteDelAutoEnCurvaTranca) ferrari `shouldBe` 0
    it "Tiempo de pista de un auto en una curva tranca" $ do
      tiempoCarrera (curvaTranca peugeot) `shouldBe` 27.5

 --Test b
  describe "Tests de rectas:" $ do
    it "Desgaste del chasis de un auto en un RetroClassic" $ do
      (desgasteChasis . desgasteDelAutoEnRectoClassic) ferrari `shouldBe` 7.15
    it "Tiempo de pista de un auto en un RetroClassic" $ do
      tiempoCarrera (tramoRectoClassic ferrari) `shouldBe` 11
    it "Desgaste del chasis de un auto en un Tramito" $ do
      (desgasteChasis . desgasteDelAutoEnTramito) ferrari `shouldBe` 2.6
    it "Tiempo de pista de un auto en un Tramito" $ do
      tiempoCarrera (tramito ferrari) `shouldBe` 4

  --Test c
  describe "Tests de zigZagLoco :" $ do
    it "Desgaste del chasis de un auto en un zigZagLoco" $ do
      (desgasteChasis . desgasteDelAutoEnZigZagLoco) ferrari `shouldBe` 5
    
    it "Desgaste del ruedas de un auto en zigZagLoco" $ do
      (desgasteRueda . desgasteDelAutoEnZigZagLoco) ferrari `shouldBe` 32.5
    
    it "Tiempo de pista de un auto en un zigZagLoco" $ do
      tiempoCarrera (zigZagLoco ferrari)  `shouldBe` 15
    
    it "Desgaste del chasis de un auto en una casiCurva" $ do
      (desgasteChasis . desgasteDelAutoEnCasiCurva) ferrari `shouldBe` 5
    
    it "Desgaste del ruedas de un auto en una casiCurva" $ do
      (desgasteRueda . desgasteDelAutoEnCasiCurva) ferrari `shouldBe` 6.5
    
    it "Tiempo de pista de un auto en un casiCurva" $ do
      tiempoCarrera (casiCurva ferrari ) `shouldBe` 3
 
  --Test d    
  describe "Tests de Rulo :" $ do
    it "Desgaste del chasis de un auto en un ruloClasico" $ do
      (desgasteChasis . desgasteDelAutoEnRuloClasico) ferrari `shouldBe` 0
    
    it "Desgaste de ruedas de un auto en un ruloClasico" $ do
      (desgasteRueda . desgasteDelAutoEnRuloClasico) ferrari `shouldBe` 19.5
    
    it "Tiempo de pista de un auto en un ruloClasico" $ do
      tiempoCarrera (ruloClasico ferrari)  `shouldBe` 1
    
    it "Desgaste del chasis de un auto en un deseoDeMuerte  " $ do
      (desgasteChasis . desgasteDelAutoEnDeseoDeMuerte) ferrari `shouldBe` 0
    
    it "Desgaste de ruedas de un auto en un deseoDeMuerte" $ do
      (desgasteRueda . desgasteDelAutoEnDeseoDeMuerte) ferrari `shouldBe` 39
    
    it "Tiempo de pista de un auto en un deseoDeMuerte" $ do
      tiempoCarrera (deseoDeMuerte ferrari)  `shouldBe` 2  

-- Test 5) Nivel de Joyez

  describe "Tests de nivel de joyez :" $ do
    it "Probamos el nivel de joyez de 3 autos" $ do
      nivelDeJoyez autos `shouldBe` 3

  describe "Tests de paraEntendidos :" $ do
    it "Probamos el para entendidos de un auto con mucho tiempo de carrera " $ do
      paraEntendidos ferraris `shouldBe` False

    it "Probamos el para entendidos con 2 autos, uno que no esta en buen estado" $ do
      paraEntendidos ferrariPeugeot `shouldBe` False

    it "Probamos el para entendidos con 2 autos que estan en buen estado y con poco tiempo de carrera " $ do
      paraEntendidos ferrariLambo `shouldBe` True
    
-- Entrega 2 

  describe "Tests Agregar autos a un equipo:" $ do
    it "Agregamos auto a un equipo con presupuesto suficiente" $ do
       buscarAuto ferrari65k (agregarAutoEquipo ferrari65k losMasRapidos)  `shouldBe` True
       buscarAuto fiat44k (agregarAutoEquipo fiat44k losMasRapidos)  `shouldBe` True
    
    it "Agregamos auto a un equipo sin presupuesto suficiente" $ do
      buscarAuto lambo73k (agregarAutoEquipo lambo73k losMasRapidos)  `shouldBe` False
     
  describe "Tests reparacion de equipo:" $ do
    it "Reparacion con presupuesto suficiente para todos los autos" $ do
      desgasteChasis (desgaste (conjuntoAutosEquipo ((repararEquipo equipoConPresupuesto)) !! 0)) `shouldBe` 1.5
      desgasteChasis (desgaste (conjuntoAutosEquipo ((repararEquipo equipoConPresupuesto)) !! 1))`shouldBe` 3
      presupuesto (repararEquipo equipoConPresupuesto) `shouldBe` 7250

    it "Reparacion sin presupuesto suficiente para ningun auto" $ do 
      desgasteChasis (desgaste (conjuntoAutosEquipo ((repararEquipo equipoPobre)) !! 0)) `shouldBe` 50 
      presupuesto (repararEquipo equipoPobre) `shouldBe` 10000

   --esta no la piden. it "Reparacion con presupuesto suficiente para algunos autos (parcialmente)" $ do 

  describe "Tests optimizar equipo (poner nitro):" $ do
    it "Poner nitro a equipo con suficiente presupuesto para todos los autos" $ do
      velocidadMaxima (conjuntoAutosEquipo (ponerNitroEquipo equipoConPresupuesto) !! 0) `shouldBe` 78
      velocidadMaxima (conjuntoAutosEquipo (ponerNitroEquipo equipoConPresupuesto) !! 1) `shouldBe` 87.6
      presupuesto (ponerNitroEquipo equipoConPresupuesto) `shouldBe` 6200
    it "Poner nitro a equipo sin suficiente presupuesto para algunos autos (parcialmente)" $ do
      velocidadMaxima (conjuntoAutosEquipo (ponerNitroEquipo equipoConPresupuestoRecortado) !! 0) `shouldBe` 78
      velocidadMaxima (conjuntoAutosEquipo (ponerNitroEquipo equipoConPresupuestoRecortado) !! 1) `shouldBe` 73
      presupuesto (ponerNitroEquipo equipoConPresupuestoRecortado) `shouldBe` 3500
           
   --esta no la piden. it Poner nitro a equipo sin suficiente presupuesto para ningun auto

  describe "Tests para ferrarizar equipo:" $ do
    it "Ferrarizar equipo con presupuesto para todos los autos" $ do
      marca ((conjuntoAutosEquipo (ferrarizar equipoPeLa)) !! 0) `shouldBe` "Ferrari"
      modelo ((conjuntoAutosEquipo (ferrarizar equipoPeLa)) !! 0) `shouldBe` "F50"
      marca ((conjuntoAutosEquipo (ferrarizar equipoPeLa)) !! 1) `shouldBe` "Ferrari"
      modelo ((conjuntoAutosEquipo (ferrarizar equipoPeLa)) !! 1) `shouldBe` "F50"
      presupuesto (ferrarizar equipoPeLa) `shouldBe` 13000
    it "Ferrarizar equipo con presupuesto para algunos autos (parcialmente)" $ do
      marca ((conjuntoAutosEquipo (ferrarizar equipoFP)) !! 0) `shouldBe` "Ferrari"
      modelo ((conjuntoAutosEquipo (ferrarizar equipoFP)) !! 0) `shouldBe` "F50"
      marca ((conjuntoAutosEquipo (ferrarizar equipoFP)) !! 1) `shouldBe` "Lambo"
      presupuesto (ferrarizar equipoFP) `shouldBe` 500
      marca ((conjuntoAutosEquipo (ferrarizar equipoPeLaFe)) !! 0) `shouldBe` "Ferrari"
      modelo ((conjuntoAutosEquipo (ferrarizar equipoPeLaFe)) !! 0) `shouldBe` "F50"
      marca ((conjuntoAutosEquipo (ferrarizar equipoPeLaFe)) !! 1) `shouldBe` "Ferrari"
      modelo ((conjuntoAutosEquipo (ferrarizar equipoPeLaFe)) !! 1) `shouldBe` "F50"
      marca ((conjuntoAutosEquipo (ferrarizar equipoPeLaFe)) !! 2) `shouldBe` "Ferrari"
      modelo ((conjuntoAutosEquipo (ferrarizar equipoPeLaFe)) !! 2) `shouldBe` "F50"
      presupuesto (ferrarizar equipoPeLaFe) `shouldBe` 13000
      

    -- no la piden it "Ferrarizar equipo sin presupuesto para ningun auto" $ do

  describe "Tests para costear reparacion de equipo:" $ do
    it "Calcular costo de raparacion de equipo completo" $ do
      costoTotalReparacion equipoConPresupuesto `shouldBe` 12750
      costoTotalReparacion equipoItaloFrances `shouldBe` 21250




  