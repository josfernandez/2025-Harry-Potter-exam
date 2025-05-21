module Library where
import PdePreludat
--import GHC.Num (Num)
--import qualified Data.Foldable as 2.2

-- Punto A Modelado de postres

data Postre = Postre {
  sabores :: [String],
  peso :: Number,
  temperatura :: Number
} deriving (Show, Eq)

-- Fin Punto A
-- Punto B Modelar Hechizos

type TipoHechizo = Postre -> Postre

incendio :: TipoHechizo
incendio postre = postre {temperatura= (modificarTemp 1 (temperatura postre)), peso= modificarPeso 0.95 (peso postre)}

modificarTemp :: Number -> Number -> Number -- si quiero subir temperatura sera un number positivo sino sera negativo
modificarTemp valor temperatura = valor + temperatura

modificarPeso :: Number -> Number -> Number -- si quiero subir 1.xx si quiero bajar 0.xx
modificarPeso porcentaje peso = porcentaje*peso

inmobulus :: TipoHechizo
inmobulus postre = postre {temperatura=0}

leviosa :: TipoHechizo
leviosa postre = postre {sabores = sabores postre ++ ["concentrado"], peso = modificarPeso 0.9 (peso postre)}

diffindo :: Number -> TipoHechizo
diffindo porcentaje postre = postre {peso= modificarPeso porcentaje (peso postre)}

ridiculus :: String -> TipoHechizo
ridiculus sabor postre = postre {sabores = sabores postre ++ [(darvuelta sabor)]}

kadavra :: TipoHechizo
kadavra = borrarSabores.inmobulus

borrarSabores :: Postre -> Postre
borrarSabores postre = postre {sabores = []}

darvuelta :: String -> String
darvuelta cadenaadarvuelta = reverse cadenaadarvuelta
--fin Punto B
--Punto C
--el postre esta listo cuando el peso es mayor a cero, tiene algun sabor, y no esta congelado

estaListo :: [Postre] -> TipoHechizo -> Bool
estaListo postres hechizo =  all postreListo (map hechizo postres) 

postreListo :: Postre -> Bool
postreListo postre = peso postre >0 && not (null (sabores postre)) && temperatura postre /= 0

pesoPromedio :: [Postre] -> Number
pesoPromedio [] = 0
pesoPromedio postres = sum (pesoTotal (postres)) / cantPostres (postres)

pesoTotal :: [Postre] -> [Number] -- esta funcion me devuelve una lista de los pesos
pesoTotal postres = map peso postres

cantPostres :: [Postre] -> Number
cantPostres postres = length postres
