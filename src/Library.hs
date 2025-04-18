module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- 1. MODELADO DE AUTO
data Desgaste = Desgaste {
    desgasteChasis :: Number,
    desgasteRueda :: Number
} deriving (Show)

data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: Desgaste,
    velocidadMaxima :: Number, -- en m/s
    tiempoCarrera :: Number,   -- en segundos
    apodo :: [String]
} deriving (Show)

-- CREACION DE AUTOS
auto = [
    Auto "Ferrari" "F50" (Desgaste 0 0) 5 0 ["La nave","El fierro","Ferrucho"],
    Auto "Lamborghini" "Diablo" (Desgaste 7 4) 73 0 ["Lambo","La bestia"],
    Auto "Fiat" "600" (Desgaste 33 27) 44 0 ["La Bocha","La bolita","Fitito"],
    Auto "Peugeot" "504" (Desgaste 0 0) 40 0 ["La Bocha","La bolita","El rey del desierto"]
 ]

-- 2. ESTADO DE SALUD DEL AUTO

-- funcion a) enBuenEstado
enBuenEstado :: Auto -> Bool
enBuenEstado auto   
    | marca auto == "Peugeot" = False
    | tiempoCarrera auto < 100 = desgasteChasis (desgaste auto) < 20
    | otherwise = desgasteChasis (desgaste auto) < 40 && desgasteRueda (desgaste auto) < 60 

-- funcion b) noDaMas
noDaMas :: Auto -> Bool
noDaMas auto = 
    (primerasDosLetrasApodo (primerApodo auto) == "La" && desgasteChasis (desgaste auto) > 80)
    || desgasteRueda (desgaste auto) > 80

primerApodo :: Auto -> String
primerApodo auto = head (apodo auto)

primerasDosLetrasApodo :: String -> String
primerasDosLetrasApodo auto = take 2 auto -- take n xs es la lista de los n primeros elementos de xs.

-- funcion c) esUnChiche

esUnChiche :: Auto -> Bool
esUnChiche auto
    | desgasteChasis (desgaste auto) < 20 && esPar (cantidadDeApodos auto) = True
    | not (esPar (cantidadDeApodos auto)) && desgasteChasis (desgaste auto) < 50 = True
    | otherwise = False 

cantidadDeApodos :: Auto -> Number
cantidadDeApodos auto = length (apodo auto)

esPar :: Number -> Bool
esPar n = even n

{-
CON COMPOSICION DE FUNCIONES

esParApodosAuto :: Auto -> Bool
esParApodosAuto auto = esPar . cantidadDeApodos
-} 

-- funcion d) esUnaJoya

esUnaJoya :: Auto -> Bool
esUnaJoya auto = (desgasteChasis (desgaste auto) == 0 ) && (desgasteRueda (desgaste auto) == 0) &&
 (cantidadDeApodos auto <= 1)

-- funcion e) nivelDeChetez

nivelDeChetez :: Auto -> Number
nivelDeChetez auto = (cantidadDeApodos auto) * 20 + cantidadDeCaracteresDeModelo auto

cantidadDeCaracteresDeModelo :: Auto -> Number
cantidadDeCaracteresDeModelo auto = length (modelo auto)

-- funcion f) supercalifragilisticaespialidosa

capacidadSuperCali :: Auto -> Number
capacidadSuperCali auto = cantLetrasPrimerApodo auto

cantLetrasPrimerApodo :: Auto -> Number
cantLetrasPrimerApodo auto = length (primerApodo auto)

-- funcion g) riesgoAuto
riegoDelAuto :: Auto -> Number
riegoDelAuto auto 
    | enBuenEstado auto = calcularRiesgo auto
    | otherwise = 2 * calcularRiesgo auto

calcularRiesgo :: Auto -> Number
calcularRiesgo auto = (velocidadMaxima auto) * (desgasteRueda (desgaste auto) / 10)

-- 3. MANOS A LA OBRA

-- funcion a) repararAuto

repararAuto :: Auto -> Auto
repararAuto auto = auto {
    desgaste = Desgaste {
        desgasteChasis = desgasteChasis (desgaste auto) * 0.15,
        desgasteRueda = 0
    } 
}

{-
Sin usar registros se escribiria asi

Auto :: String -> String -> Desgaste -> Number -> Number -> [String] -> Auto
Desgaste :: Number -> Number -> Desgaste

repararAuto :: Auto -> Auto
repararAuto auto = Auto
    (marca auto)
    (modelo auto)
    (Desgaste (desgasteChasis (desgaste auto) * 0.15) 0)
    (velocidadMaxima auto)
    (tiempoCarrera auto)
    (apodo auto)
-}

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

