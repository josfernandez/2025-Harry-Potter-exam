module Library where
import PdePreludat
import Test.Hspec.Discover (postProcessSpec)
--import GHC.Num (Num)
--import qualified Data.Foldable as 2.2


--PUNTO A
data Postre = Postre {
  sabores :: [String],
  peso :: Number,
  temp :: Number
} deriving (Show, Eq)
--FIN PUNTO A
--PUNTO B
type Hechizo = Postre -> Postre

cambiarPeso :: Number -> Postre -> Postre
cambiarPeso porcentaje postre = postre {peso=(peso postre)*porcentaje}

cambiarTemp :: Number -> Postre -> Postre
cambiarTemp grados postre = postre{temp=(temp postre)+grados}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre= postre{sabores=sabor:sabores postre}

invertirPostre :: String -> String
invertirPostre postre = reverse postre 

borrarSabores :: Postre -> Postre
borrarSabores postre= postre{sabores=[]}

incendio :: Hechizo
incendio = cambiarPeso (1.05).cambiarTemp (1)

immobulus :: Hechizo
immobulus postre = postre {temp=0}

wingardium :: Hechizo
wingardium = cambiarPeso 0.9.agregarSabor "Concentrado"

diffindo :: Number -> Hechizo
diffindo porcentaje = cambiarPeso porcentaje

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (invertirPostre sabor) 

avada :: Hechizo
avada = immobulus.borrarSabores
--FIN PUNTO B
--PUNTO C
estaNListos :: [Postre] -> Hechizo -> Bool
estaNListos postres hechizo = all (estaListo.hechizo) postres

estaListo :: Postre -> Bool
estaListo postre = peso postre>0 && sabores postre /= [] && temp postre/=0
--FIN PUNTO C
--PUNTO D
--DADO UN CONJUNTO DE POSTRES, CONOCER EL PESO PROMEDIO DE LOS POSTRES LISTOS
listaPromedio :: [Postre] -> [Number]--me devuelve una lista con los pesos listos
listaPromedio (primero:resto) 
                              | estaListo primero = peso primero : listaPromedio resto
                              | otherwise = listaPromedio resto

pesoPromedio :: [Number] -> Number--toma la lista de pesos listos y calcula el promedio
pesoPromedio lista = sum lista / length lista

calcularPromedioListo :: [Postre] -> Number
calcularPromedioListo = pesoPromedio.listaPromedio
--FIN PUNTO D
--PARTE 2
--PUNTO A
--FUNCION QUE RECIBE MAGO, HECHIZO Y DEVUELVE UN MAGO
data Mago = Mago {
  hechizos :: [Hechizo],
  horrocruxes :: Number
} deriving (Show, Eq)

entrenarMago :: Mago -> Postre -> Hechizo -> Mago
entrenarMago mago postre hechizo 
                                  | (hechizo postre==avada postre) = mago{hechizos=hechizo:hechizos mago,horrocruxes=horrocruxes mago+1}
                                  | otherwise = mago{hechizos=hechizo:hechizos mago}
--FIN PUNTO A
--PUNTO B
--DADO UN POSTRE UN MAGO, OBTENER EL MEJOR HECHIZO, ES AQUEL QUE DEJA AL POSTRE CON MAS SABORES

mejorHechizodelMago :: Postre -> Mago -> Hechizo
mejorHechizodelMago postre mago = mejorHechizo (hechizos mago) postre

mejorHechizo :: [Hechizo] -> Postre -> Hechizo
mejorHechizo [unico] _ = unico --si la lista tiene un unico hechizo devuelvo ese hechizo unico, o el mejor
mejorHechizo (primero:segundo:resto) postre 
                                            | elMejor primero segundo postre = mejorHechizo (primero:resto) postre 
                                            | otherwise = mejorHechizo (segundo:resto) postre 
                                            
elMejor :: Hechizo -> Hechizo -> Postre -> Bool
elMejor hechizo1 hechizo2 postre 
                                  | length (sabores (hechizo1 postre))>length (sabores (hechizo2 postre)) = True
                                  | otherwise = False
                              
--FIN PUNTO B
--PUNTO 3
--A

postreModelo :: Postre
postreModelo = Postre {sabores=["Limon"],peso=10,temp=10}

postreInfinito :: [Postre]
postreInfinito = repeat postreModelo

hechizoInfinito :: [Hechizo]
hechizoInfinito = repeat incendio

magoModelo :: Mago
magoModelo = Mago{hechizos=hechizoInfinito,horrocruxes=0}
--fin punto A
--B
--si, esta la funcion estaNListos, la funcion all es lazy evaluation, esto significa que no evalua la lista completa, sino que evalua 
--hasta encontrar el primer False, una vez que obtiene el primer False, deja de evaluar 

--C no, porque para saber si es el mejor hechizo, tiene que comparar todos los hechizos para quedarse con el mejor.
-- lo que se puede hacer es recortar (take n hechizos mago) la cantidad de hechizos del mago, haciendo una cantidad finita.
