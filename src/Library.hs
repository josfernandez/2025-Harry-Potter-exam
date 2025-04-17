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

enBuenEstado :: Auto -> Bool
enBuenEstado auto   
    | marca auto == "Peugeot" = False
    | tiempoCarrera auto < 100 = desgasteChasis (desgaste auto) < 20
    | otherwise = desgasteChasis (desgaste auto) < 40 && desgasteRueda (desgaste auto) < 60 

noDaMas :: Auto -> Bool
noDaMas auto = 
    (primerasDosLetrasApodo (primerApodo auto) == "La" && desgasteChasis (desgaste auto) > 80)
    || desgasteRueda (desgaste auto) > 80

primerApodo :: Auto -> String
primerApodo auto = head (apodo auto)

primerasDosLetrasApodo :: String -> String
primerasDosLetrasApodo auto = take 2 auto -- take n xs es la lista de los n primeros elementos de xs.