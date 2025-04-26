module Library where
import PdePreludat

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
desgasteDeChasisAuto auto = desgasteChasis (desgaste auto)

desgasteDeRuedaAuto :: Auto -> Number
desgasteDeRuedaAuto auto = desgasteRueda (desgaste auto)

-- funcion b) noDaMas
noDaMas :: Auto -> Bool
noDaMas auto = 
    ((empiezaConLa.primerApodo) auto && desgasteDeChasisAuto auto > 80) || desgasteDeRuedaAuto auto > 80

primerApodo :: Auto -> String
primerApodo auto = head (apodo auto)

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
cantidadDeApodos auto = (length.apodo) auto

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
cantidadDeCaracteresDeModelo auto = (length.modelo) auto

-- funcion f) supercalifragilisticaespialidosa

capacidadSuperCali :: Auto -> Number
capacidadSuperCali auto = cantLetrasPrimerApodo auto

cantLetrasPrimerApodo :: Auto -> Number
cantLetrasPrimerApodo auto = (length.primerApodo) auto

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
bautizarElAuto nombreBautismo auto = auto {apodo = apodo auto ++ [nombreBautismo]} -- ++ agrega al final de la lista

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