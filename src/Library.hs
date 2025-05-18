module Library where
import PdePreludat
import GHC.Num (Num)
--import qualified Data.Foldable as 2.2

doble :: Number -> Number
doble numero = numero + numero

-- 1. MODELADO DE AUTO
data Desgaste = Desgaste {
    desgasteChasis :: Number,
    desgasteRueda :: Number
} deriving (Show, Eq)

data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: Desgaste,
    velocidadMaxima :: Number, -- en m/s
    tiempoCarrera :: Number,   -- en segundos
    apodo :: [String]
} deriving (Show, Eq)

-- CREACION DE AUTOS

ferrari :: Auto
ferrari = Auto "Ferrari" "F50" (Desgaste 0 0) 65 0 ["La nave","El fierro","Ferrucho"]

lamborghini :: Auto
lamborghini = Auto "Lamborghini" "Diablo" (Desgaste 7 4) 73 0 ["Lambo","La bestia"]

fiat :: Auto
fiat = Auto "Fiat" "600" (Desgaste 33 27) 44 0 ["La Bocha","La bolita","Fitito"]

peugeot :: Auto
peugeot = Auto "Peugeot" "504" (Desgaste 0 0) 40 0 ["La Bocha","La bolita","El rey del desierto"] 


-- 2. ESTADO DE SALUD DEL AUTO

-- funcion a) enBuenEstado
enBuenEstado :: Auto -> Bool
enBuenEstado Auto{marca = "Peugeot"} = False
enBuenEstado auto   
    | tiempoCarrera auto < 100 = desgasteDeChasisAuto auto < 20
    | otherwise = desgasteDeChasisAuto auto < 40 && desgasteDeRuedaAuto auto < 60 

desgasteDeChasisAuto :: Auto -> Number
desgasteDeChasisAuto = desgasteChasis . desgaste

desgasteDeRuedaAuto :: Auto -> Number
desgasteDeRuedaAuto = desgasteRueda . desgaste 

-- funcion b) noDaMas
noDaMas :: Auto -> Bool
noDaMas auto = 
    ((empiezaConLa.primerApodo) auto && desgasteDeChasisAuto auto > 80) || desgasteDeRuedaAuto auto > 80

primerApodo :: Auto -> String
primerApodo = head . apodo 

empiezaConLa :: String -> Bool
empiezaConLa auto = take 2 auto == "La" -- take n xs es la lista de los n primeros elementos de xs.

-- funcion c) esUnChiche

esUnChiche :: Auto -> Bool
esUnChiche auto =
    (esPar cantApodos && desgasteChasis < 20) ||
    (not (esPar cantApodos) && desgasteChasis < 50)
  where
    cantApodos = cantidadDeApodos auto
    desgasteChasis = desgasteDeChasisAuto auto
 
cantidadDeApodos :: Auto -> Number
cantidadDeApodos  = length . apodo

esPar :: Number -> Bool
esPar n = even n

-- funcion d) esUnaJoya

esUnaJoya :: Auto -> Bool
esUnaJoya auto = (desgasteDeChasisAuto auto == 0 ) && (desgasteDeRuedaAuto auto == 0) &&
 (cantidadDeApodos auto <= 1)

-- funcion e) nivelDeChetez

nivelDeChetez :: Auto -> Number
nivelDeChetez auto = (cantidadDeApodos auto) * 20 * cantidadDeCaracteresDeModelo auto

cantidadDeCaracteresDeModelo :: Auto -> Number
cantidadDeCaracteresDeModelo = (length.modelo) 

-- funcion f) supercalifragilisticaespialidosa

capacidadSuperCali :: Auto -> Number
capacidadSuperCali = cantLetrasPrimerApodo 

cantLetrasPrimerApodo :: Auto -> Number
cantLetrasPrimerApodo = (length.primerApodo)

-- funcion g) riesgoAuto
riesgoDelAuto :: Auto -> Number
riesgoDelAuto auto 
    | enBuenEstado auto = calcularRiesgo auto
    | otherwise = 2 * calcularRiesgo auto

calcularRiesgo :: Auto -> Number
calcularRiesgo auto = (velocidadMaxima auto) * (desgasteDeRuedaAuto auto / 10)

-- 3. MANOS A LA OBRA

-- funcion a) repararAuto

repararAuto :: Auto -> Auto
repararAuto auto = auto {
    desgaste = Desgaste {
        desgasteChasis = desgasteDeChasisAuto auto * 0.15,
        desgasteRueda = 0
    } 
}

-- funcion b) aplicarPenalidad

aplicarPenalidad :: Number -> Auto -> Auto
aplicarPenalidad segundos auto = auto {tiempoCarrera = tiempoCarrera auto + segundos}

-- funcion c) ponerleNitro

ponerleNitro :: Auto -> Auto
ponerleNitro auto = auto {velocidadMaxima = velocidadMaxima auto * 1.2}

-- funcion d) bautizarElAuto

bautizarElAuto :: String -> Auto -> Auto
bautizarElAuto nombreBautismo auto = auto {apodo = apodo auto ++ [nombreBautismo] } 

-- ++ agrega al final de la lista

-- funcion e) llevarAutoADesarmadero

llevarAutoADesarmadero :: String -> String -> Auto -> Auto
llevarAutoADesarmadero nuevaMarca nuevoModelo auto = auto { 
    marca = nuevaMarca,
    modelo = nuevoModelo,
    apodo = ["Nunca Taxi"]
}

-- 4. PISTAS!!!

data Pista = Pista {
    nombre :: String,
    pais :: String,
    precioEntrada :: Number,
    tramos :: [Tramo] 
}

type Tramo = Auto -> Auto -- recibo un auto y al pasar por el tramo, modifica algo (devuelve Auto')

-- Modelo de Curva

curva :: Number -> Number -> Tramo
curva angulo longitud auto = auto {
  desgaste = (desgaste auto) {
    desgasteRueda = desgasteDeRuedaAuto auto + desgasteRuedaEnCurva longitud angulo
  },
  tiempoCarrera = tiempoCarrera auto + aumentoTiempoEnCurva longitud auto
}

desgasteRuedaEnCurva :: Number -> Number -> Number
desgasteRuedaEnCurva longitud angulo = 3 * longitud / angulo

aumentoTiempoEnCurva :: Number -> Auto -> Number
aumentoTiempoEnCurva longitud auto = longitud / (velocidadMaxima auto / 2)

--Desgaste del auto que paso una CurvaPeligrosa
desgasteDelAutoEnCurvaPeligrosa :: Auto -> Desgaste
desgasteDelAutoEnCurvaPeligrosa = desgaste . curvaPeligrosa

--Desgaste del auto que paso una CurvaTranca
desgasteDelAutoEnCurvaTranca :: Auto -> Desgaste
desgasteDelAutoEnCurvaTranca = desgaste . curvaTranca

-- Modelo particularidades de curvas solicitadas

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

curvaTranca :: Tramo
curvaTranca = curva 110 550

-- Modelo de Recta

recta :: Number -> Tramo
recta longitud auto = auto {
  desgaste = (desgaste auto) {
    desgasteChasis = desgasteDeChasisAuto auto + longitud / 100
  },
  tiempoCarrera = tiempoCarrera auto + aumentoTiempoEnRecta longitud auto 
}

aumentoTiempoEnRecta :: Number -> Auto -> Number
aumentoTiempoEnRecta longitud auto = longitud / velocidadMaxima auto

--Desgaste del auto que paso un rectoClassic
desgasteDelAutoEnRectoClassic :: Auto -> Desgaste
desgasteDelAutoEnRectoClassic = desgaste . tramoRectoClassic 

--Desgaste del auto que paso un tramito
desgasteDelAutoEnTramito :: Auto -> Desgaste
desgasteDelAutoEnTramito = desgaste . tramito 

-- Modelo particularidades de rectas solicitadas

tramoRectoClassic :: Tramo
tramoRectoClassic = recta 715

tramito :: Tramo
tramito = recta 260

-- Modelo Zig Zag

zigzag :: Number -> Tramo
zigzag cambios auto = auto {
  desgaste = (desgaste auto) {
    desgasteChasis = desgasteDeChasisAuto auto + 5,
    desgasteRueda = desgasteDeRuedaAuto auto + desgasteRuedaZigZag cambios auto
  },
  tiempoCarrera = tiempoCarrera auto + cambios * 3
}

desgasteRuedaZigZag :: Number -> Auto -> Number
desgasteRuedaZigZag cambios auto = (velocidadMaxima auto * cambios / 10)

--Desgaste del auto que paso un zigZagLoco
desgasteDelAutoEnZigZagLoco :: Auto -> Desgaste
desgasteDelAutoEnZigZagLoco = desgaste . zigZagLoco 

--Desgaste del auto que paso un casiCurva
desgasteDelAutoEnCasiCurva :: Auto -> Desgaste
desgasteDelAutoEnCasiCurva = desgaste . casiCurva 


-- Modelo particularidades de ZigZag solicitadas

zigZagLoco :: Tramo
zigZagLoco = zigzag 5

casiCurva :: Tramo
casiCurva = zigzag 1

-- Modelo Rulo

rulo :: Number -> Tramo
rulo diametro auto = auto {
  desgaste = (desgaste auto) {
    desgasteRueda = desgasteDeRuedaAuto auto + diametro * 1.5
  },
  tiempoCarrera = tiempoCarrera auto + aumentaTiempoEnRulo diametro auto
}

aumentaTiempoEnRulo :: Number -> Auto -> Number 
aumentaTiempoEnRulo diametro auto = (5 * diametro) / velocidadMaxima auto

--Desgaste del auto que paso un ruloClasico
desgasteDelAutoEnRuloClasico :: Auto -> Desgaste
desgasteDelAutoEnRuloClasico = desgaste . ruloClasico 

--Desgaste del auto que paso un deseoDeMuerte
desgasteDelAutoEnDeseoDeMuerte :: Auto -> Desgaste
desgasteDelAutoEnDeseoDeMuerte = desgaste . deseoDeMuerte 

-- Modelo particularidades de Rulos solicitados

ruloClasico :: Tramo
ruloClasico = rulo 13

deseoDeMuerte :: Tramo
deseoDeMuerte = rulo 26


-- 5. Nivel de Joyez

-- a) Nivel de joyez de un grupo de autos

nivelDeJoyez :: [Auto] -> Number
nivelDeJoyez = sum . (map joyezIndividual) 

joyezIndividual :: Auto -> Number
joyezIndividual auto
    | tiempoCarrera auto < 50 = 1
    | esUnaJoya auto = 2
    | otherwise = 0

-- b) Para entendidos de un grupo de autos

paraEntendidos :: [Auto] -> Bool
paraEntendidos = all cumpleCondicionEntendido

cumpleCondicionEntendido :: Auto -> Bool
cumpleCondicionEntendido auto = tiempoCarrera auto <= 200 && enBuenEstado auto

-- PARTE 2

--Funciones utils
-- Se utiliza como funcion base para calcular el presupesto restante de todas las modificaicones
presupuestoRestante :: (Auto -> Number) -> [Auto] -> Number   -> Number
presupuestoRestante calcularCosto [] p  = p
presupuestoRestante calcularCosto (a:as) p 
  | calcularCosto a <= p = presupuestoRestante calcularCosto as (p - calcularCosto a) 
  | otherwise = presupuestoRestante calcularCosto as p 



-- 2.1 Equipos de Competicion

data Equipo = Equipo {
    nombreEquipo :: String,
    conjuntoAutosEquipo :: [Auto],
    presupuesto :: Number
} deriving (Show, Eq)

--2.1.a. Agregar autos a equipos

losMasRapidos :: Equipo 
losMasRapidos = Equipo "losMasRapidos" [] 70000

--equipos y autos de prueba
ferraric10p20 = Auto "Ferrari" "F40" (Desgaste 10 0) 65 0 []
lamboc20p20 = Auto "Lambo" "sv" (Desgaste 20 0) 73 0 []
fiatc50p10 = Auto "Fiat" "argos" (Desgaste 50 0) 50 0 []

noTanRapidos :: Equipo
noTanRapidos = Equipo "Tortugas" [ferraric10p20,lamboc20p20] 20000

fanFiat :: Equipo
fanFiat = Equipo "FIATCHINI" [fiatc50p10] 10000


--equipos y autos de prueba

agregarAutoEquipo :: Auto -> Equipo -> Equipo
agregarAutoEquipo auto equipo
  | calcularCostoAuto auto <= presupuesto equipo = Equipo {
      nombreEquipo = nombreEquipo equipo,
      conjuntoAutosEquipo = auto : conjuntoAutosEquipo equipo, -- agrega el nuevo auto al inicio de la lista
      presupuesto = presupuesto equipo - calcularCostoAuto auto
    }
  | otherwise = equipo

calcularCostoAuto :: Auto -> Number
calcularCostoAuto auto = velocidadMaxima auto * 1000 

-- 2.1.b. Realizar una Reparacion

{-
USO LA FUNCION YA CREADA
repararAuto :: Auto -> Auto
repararAuto auto = auto { desgasteDeChasisAuto = desgasteDeChasisAuto auto * 0.15 }
-}

repararEquipo :: Equipo -> Equipo
repararEquipo equipo = equipo {
  conjuntoAutosEquipo = autosReparados (conjuntoAutosEquipo equipo) (presupuesto equipo),
  presupuesto = presupuestoRestanteDeReparacion (conjuntoAutosEquipo equipo) (presupuesto equipo) 
}


autosReparados :: [Auto] -> Number -> [Auto]
autosReparados [] _ = [] -- caso base que faltaba
autosReparados (a:as) p
  | calcularCostoReparacion a <= p = repararAuto a : autosReparados as (p - calcularCostoReparacion a)
  | otherwise = a : autosReparados as p


--Presupuesto Restante de la reparacion
presupuestoRestanteDeReparacion :: [Auto] -> Number -> Number
presupuestoRestanteDeReparacion = presupuestoRestante calcularCostoReparacion

calcularCostoReparacion :: Auto -> Number
calcularCostoReparacion auto = (desgasteDeChasisAuto auto - desgasteDeChasisAuto auto * 0.15) * 500


-- 2.1.c. Optimizar autos 

ponerNitroEquipo :: Equipo -> Equipo
ponerNitroEquipo equipo = equipo {
  conjuntoAutosEquipo = ponerNitroAutos equipo,
  presupuesto = presupuestoRestanteNitro (conjuntoAutosEquipo equipo) (presupuesto equipo)
}

ponerNitroAutos:: Equipo -> [Auto]
ponerNitroAutos equipo = autosConNitro (conjuntoAutosEquipo equipo) (presupuesto equipo)

autosConNitro :: [Auto] -> Number -> [Auto]
autosConNitro [] _ = []   -- caso base que faltaba
autosConNitro (auto:autos) presupuesto
  | gastoEnNitro auto <= presupuesto = ponerleNitro auto : autosConNitro autos (presupuesto - gastoEnNitro auto)
  | otherwise = auto : autosConNitro autos presupuesto

--Presupuesto Restante de poner el nitro
presupuestoRestanteNitro :: [Auto] -> Number -> Number
presupuestoRestanteNitro = presupuestoRestante gastoEnNitro

gastoEnNitro :: Auto -> Number
gastoEnNitro auto = velocidadMaxima auto * 100


-- 2.1.d. Ferrarizar

ferrarizar :: Equipo -> Equipo
ferrarizar equipo = equipo {
  conjuntoAutosEquipo = cambiarPorFerraris (conjuntoAutosEquipo equipo) (presupuesto equipo),
  presupuesto = presupuestoRestanteFerrarizacion (conjuntoAutosEquipo equipo) (presupuesto equipo)
}

cambiarPorFerraris :: [Auto] -> Number -> [Auto]
cambiarPorFerraris [] _ = []  -- caso base faltante
cambiarPorFerraris (auto:autos) presupuesto
  | gastoDeFerrarizacion auto <= presupuesto = convertirloAFerrari auto : cambiarPorFerraris autos (presupuesto - gastoDeFerrarizacion auto)
  | otherwise = auto : cambiarPorFerraris autos presupuesto

convertirloAFerrari :: Auto -> Auto
convertirloAFerrari = llevarAutoADesarmadero "Ferrari" "F50" 

--Presupuesto Restante de poner el nitro
presupuestoRestanteFerrarizacion :: [Auto] -> Number -> Number
presupuestoRestanteFerrarizacion = presupuestoRestante gastoDeFerrarizacion

gastoDeFerrarizacion:: Auto -> Number
gastoDeFerrarizacion auto 
  | marca auto == "Ferrari" = 0
  | otherwise = 3500


--2.2 Funcione Utils
costoTotal :: ([Auto] -> Number -> Number) -> Equipo -> Number
costoTotal presupuestoRestante equipo = presupuesto equipo - presupuestoRestante (conjuntoAutosEquipo equipo) (presupuesto equipo)

--2.2.a Total costo de reparacion
costoTotalReparacion :: Equipo -> Number
costoTotalReparacion  = costoTotal presupuestoRestanteDeReparacion 

--funciones para testing
buscarAuto :: Auto -> Equipo -> Bool
buscarAuto auto equipo = auto `elem` (conjuntoAutosEquipo equipo)

--Punto 3
--a
ferrariBase :: Auto
ferrariBase = Auto "Ferrari" "F40" (Desgaste 1 0) 300 0 []

infinia :: Equipo
infinia = Equipo "Infinia" autosInfinia 5000

autosInfinia :: [Auto]
autosInfinia = autosConVelocidad 1 (Auto "Ferrari" "F40" (Desgaste 1 0) 300 0 [])

autosConVelocidad :: Number -> Auto -> [Auto]
autosConVelocidad multiplicador base =
  base { velocidadMaxima = velocidadMaxima base * multiplicador } : autosConVelocidad (multiplicador + 1) base

--funcion extra punto 3
recortarEquiposInfinitos :: Number -> Equipo -> Equipo
recortarEquiposInfinitos corte equipo = equipo { conjuntoAutosEquipo = take corte (conjuntoAutosEquipo equipo) }

--b
{-si se realiza una reparacion de este equipo, al tener una lista infinita, va a intentar evaluar la lista completa
y nunca terminara
-}
{- pasaria lo mismo que en el punto anterior, quedaria tratando de calcular la lista completa para despues ponerles nitro
-}
{- pasaria lo mismo que en los dos puntos anteriores, no termina nunca de calcular la lista
 lo mismo que el punto anterior, no termina de calcular jamas la lista y por ende no ejecuta el resto de la funcion
-}

{-ALTERNATIVA PARA i,ii,iii,iv
si quiesieramos reparar todo el equipo, al tener una lista infinita no podemos, pero lo que podemos hacer
 es pasarle un numero finito, cortar la lista. 

repararEquipo (recortarEquiposInfinitos 5 infinia)
ponerNitroEquipo (recortarEquiposInfinitos 5 infinia)
ferrarizar (recortarEquiposInfinitos 5 infinia)
costoTotalReparacion (recortarEquiposInfinitos 5 infinia)
-}