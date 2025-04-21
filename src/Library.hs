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
enBuenEstado auto   
    | marca auto == "Peugeot" = False
    | tiempoCarrera auto < 100 = desgasteChasis (desgaste auto) < 20
    | otherwise = desgasteChasis (desgaste auto) < 40 && desgasteRueda (desgaste auto) < 60 

-- funcion b) noDaMas
noDaMas :: Auto -> Bool
noDaMas auto = 
    (primerasDosLetrasApodo (primerApodo auto) == "La" && desgasteChasis (desgaste auto) > 80) || desgasteRueda (desgaste auto) > 80

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
nivelDeChetez auto = (cantidadDeApodos auto) * 20 * cantidadDeCaracteresDeModelo auto

cantidadDeCaracteresDeModelo :: Auto -> Number
cantidadDeCaracteresDeModelo auto = length (modelo auto)

-- funcion f) supercalifragilisticaespialidosa

capacidadSuperCali :: Auto -> Number
capacidadSuperCali auto = cantLetrasPrimerApodo auto

cantLetrasPrimerApodo :: Auto -> Number
cantLetrasPrimerApodo auto = length (primerApodo auto)

-- funcion g) riesgoAuto
riesgoDelAuto :: Auto -> Number
riesgoDelAuto auto 
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

-- 4. PISTAS!!!

--Lamborghini sin apodo
lamborghiniSinApodo :: Auto
lamborghiniSinApodo = Auto "Lamborghini" "Diablo" (Desgaste 7 4) 73 0 []

-- Modelado de tramos

data Tramo
  = Curva {
      anguloCurva :: Number,
      longitudCurva :: Number
    }
  | Recto {
      longitudRecto :: Number
    }
  | ZigZag {
      cambiosDeDireccion :: Number
    }
  | Rulo {
      diametro :: Number
    }

-- Modelado de Pista

data Pista = Pista {
    nombre :: String,
    pais :: String,
    precioEntrada :: Number,
    tramos :: [Tramo] 
}

-- creo Instancias del tipo de dato "Tramo" usando el constructor "Curva"

curvaPeligrosa :: Tramo
curvaPeligrosa = Curva {
    anguloCurva = 60,
    longitudCurva = 300
}

curvaTranca :: Tramo
curvaTranca = Curva {
    anguloCurva = 110,
    longitudCurva = 550
}

-- Desgaste por transitar tramo curvo
desgasteEnCurva :: Auto -> Tramo -> Desgaste
desgasteEnCurva auto (Curva angulo longitud) = Desgaste {desgasteChasis = (desgasteChasis . desgaste) auto, desgasteRueda = (desgasteRueda. desgaste) auto + 3 * longitud / angulo }
desgasteEnCurva auto _ = desgaste auto

-- Desgaste en ruedas por transitar tramo curvo
desgasteDelAutoEnCurva :: Auto -> Tramo -> Auto
desgasteDelAutoEnCurva auto curva = auto {desgaste = desgasteEnCurva auto curva }

-- Tiempo que tarda en cruzar la curva
tiempoEnCurva :: Auto ->Tramo -> Number
tiempoEnCurva auto (Curva angulo longitud) = longitud / (velocidadMaxima auto / 2)


tiempoDelAutoEncurva :: Auto -> Tramo -> Number
tiempoDelAutoEncurva auto curva = tiempoCarrera auto {tiempoCarrera = tiempoCarrera auto + tiempoEnCurva auto curva }

-- creo Instancias del tipo de dato "Tramo" usando el constructor "Recto"

tramoRectoClassic :: Tramo
tramoRectoClassic = Recto {
    longitudRecto = 715    
}

tramito :: Tramo
tramito = Recto {
    longitudRecto = 260    
}

-- Desgaste por transitar tramo recto
desgasteEnRecto :: Auto -> Tramo -> Desgaste
desgasteEnRecto auto (Recto longitud) = Desgaste {desgasteChasis = (desgasteChasis . desgaste) auto + (longitud/100), desgasteRueda = (desgasteRueda. desgaste) auto }
desgasteEnRecto auto _ = desgaste auto

-- Desgaste en chasis por transitar tramo recto
desgasteDelAutoEnRecto :: Auto -> Tramo -> Auto
desgasteDelAutoEnRecto auto recto = auto {desgaste = desgasteEnRecto auto recto }

-- Tiempo que tarda en cruzar la curva
tiempoEnRecto :: Auto ->Tramo -> Number
tiempoEnRecto auto (Recto longitud) = longitud / velocidadMaxima auto 


tiempoDelAutoEnRecto :: Auto -> Tramo -> Number
tiempoDelAutoEnRecto auto recto = tiempoCarrera auto {tiempoCarrera = tiempoCarrera auto + tiempoEnRecto auto recto }

-- creo Instancias del tipo de dato "Tramo" usando el constructor "ZigZag"

zigZagLoco :: Tramo
zigZagLoco = ZigZag {
    cambiosDeDireccion = 5
}

casiCurva :: Tramo
casiCurva = ZigZag {
    cambiosDeDireccion = 1
}

-- Desgaste en chasis por transitar tramo ZigZag

-- Desgaste en ruedas por transitar tramo ZigZag

-- creo Instancias del tipo de dato "Tramo" usando el constructor "ZigZag"

ruloClasico :: Tramo
ruloClasico = Rulo {
    diametro = 13
}

deseoDeMuerte :: Tramo
deseoDeMuerte = Rulo {
    diametro = 26
}

-- Desgaste en ruedas por transitar tramo Rulo


-- 5. Nivel de Joyez

-- a) Nivel de joyez de un grupo de autos

nivelDeJoyez :: [Auto] -> Number
nivelDeJoyez autos = sum (map joyezIndividual autos)

joyezIndividual :: Auto -> Number
joyezIndividual auto 
    | (esUnaJoya auto && tiempoCarrera auto < 50) = 1
    | esUnaJoya auto = 2
    | otherwise = 0

-- b) Para entendidos de un grupo de autos

paraEntendidos :: [Auto] -> Bool
paraEntendidos autos = any noCumpleCondicionEntendido autos

noCumpleCondicionEntendido :: Auto -> Bool
noCumpleCondicionEntendido auto = tiempoCarrera auto > 200 || not (enBuenEstado auto)