module Library where
import PdePreludat

-- 1. POSTRES

-- En el universo local de Harry Postre, para hacer postres se utilizan hechizos que se van usando 
-- sobre los mismos para irlos preparando

-- A) Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a cierta temperatura.
-- Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C.

data Postre = UnPostre {
    nombre :: String,
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
} deriving(Show, Eq)

type Sabor = String

bizcochoBorracho :: Postre
bizcochoBorracho = UnPostre "Bizcocho Borracho" ["fruta","crema"] 100 25

tartaDeMelaza :: Postre
tartaDeMelaza = UnPostre "Tarta de Melaza" ["melaza"] 50 0

-- B) Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código existente. 
-- RECORDAR: Los hechizos se utilizan sobre los postres para irlos preparando

type Hechizo = Postre -> Postre

-- Funciones Auxiliares

calentar :: Number -> Postre -> Postre
calentar valor postre = postre {temperatura = temperatura postre + valor}

perderPesoEnPorcentaje :: Number -> Postre -> Postre
perderPesoEnPorcentaje porcentaje postre = postre {peso = peso postre * ((100 - porcentaje) / 100)} 

congelar :: Postre -> Postre
congelar postre = (flip calentar postre . negate . temperatura) postre

congelar' :: Postre -> Postre
congelar' postre = postre {temperatura = 0}

agregarSabor :: String -> Postre -> Postre
agregarSabor nuevoSabor postre = postre {sabores = nuevoSabor : sabores postre}

perderTodosLosSabores :: Postre -> Postre
perderTodosLosSabores postre = postre {sabores = []}

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor = not . null . sabores

estaCongelado :: Postre -> Bool
estaCongelado = (<= 0) . temperatura

-- Por ahora existen los siguientes HECHIZOS:

-- Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.

incendio :: Hechizo
incendio = perderPesoEnPorcentaje 5 . calentar 1

-- Immobulus: congela el postre, llevando su temperatura a 0.

immobulus :: Hechizo
immobulus = congelar 

-- Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. 
-- Además, pierde 10% de su peso.

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = perderPesoEnPorcentaje 10 . agregarSabor "concentrado"

-- Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado.

diffindo :: Number -> Hechizo
diffindo = perderPesoEnPorcentaje 

-- Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que tiene un postre, pero invertido.

riddikulus :: String -> Hechizo
riddikulus = agregarSabor . reverse 

-- Avada kedavra: Hace lo mismo que el immobulus pero además hace que el postre pierda todos sus sabores.

avadaKedavra :: Hechizo
avadaKedavra = perderTodosLosSabores . immobulus

-- C) Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará listos 
-- (un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está congelado).

-- Por ejemplo, si en la mesa está el bizcocho mencionado anteriormente y una tarta de melaza de 0 grados y 50 gramos, 
-- y les hago el hechizo incendio, quedan listos, pero si les hago el hechizo riddikulus con el sabor “nomil” no, 
-- porque la tarta sigue congelada.

losDejaraListos :: Hechizo -> [Postre] -> Bool
losDejaraListos hechizo =  all (estaListo) . map (\postre -> hechizo postre) 
--                         all (estaListo . hechizo) 

estaListo :: Postre -> Bool
estaListo postre = peso postre > 0 && tieneAlgunSabor postre && not (estaCongelado postre) 

-- D) Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos. 

pesoPromedioPostresListos :: [Postre] -> Number
pesoPromedioPostresListos = pesoPromedio . map peso . filter estaListo
-- 1ero. Filtro los postres listos
-- 2dos. Los transformo en una lista de pesos
-- 3ero. Realizo el pesoPromedio

pesoPromedio :: [Number] -> Number
pesoPromedio pesos = sum pesos / length pesos

-- 2. MAGOS

-- De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene.

data Mago = UnMago {
    nombreMago :: String,
    hechizos :: [Hechizo],
    cantHorrorcruxes :: Number
}deriving(Show, Eq)

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo nuevoHechizo mago = mago {hechizos = nuevoHechizo : hechizos mago}

sumarHorrorcruxes :: Number -> Mago -> Mago
sumarHorrorcruxes valor mago = mago {cantHorrorcruxes = cantHorrorcruxes mago + valor}

-- A) Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un postre 
-- (se espera obtener el mago). Cuando un mago practica con un hechizo, lo agrega a sus hechizos aprendidos. 
-- Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, 
-- entonces suma un horrorcrux.

asistirAClase :: Hechizo -> Postre -> Mago -> Mago
asistirAClase hechizo postre = resultadoHechizo hechizo postre . agregarHechizo hechizo   
-- 1ero. Agrego el hechizo a los hechizos del mago
-- 2dos. Me fijo si sumar o no un horrorcrux, en relacion al resultado de usar el hechizo

resultadoHechizo :: Hechizo -> Postre -> Mago -> Mago
resultadoHechizo hechizo postre mago
    | hechizo postre == avadaKedavra postre = sumarHorrorcruxes 1 mago
    | otherwise                             = mago

-- B) Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más 
-- cantidad de sabores luego de usarlo.

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (mejorHechizoEntreDos postre) (hechizos mago)
-- Hice un foldl1 donde la semilla es el primer hechizo de la lista de hechizos del mago
-- Luego lo que va a ir haciendo el foldl es ir comparando quien es el mejor de los hechizos
-- entre parejas hasta llegar al final de la lista de hechizos. Obteniendo como resultado mejor hechizo!!

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = (length . sabores . hechizo1) postre >= (length . sabores . hechizo2) postre

mejorHechizoEntreDos :: Postre -> Hechizo -> Hechizo -> Hechizo
mejorHechizoEntreDos postre hechizo1 hechizo2
    | esMejor postre hechizo1 hechizo2 = hechizo1
    | otherwise                        = hechizo2    

-- 3. INFINITA MAGIA

-- A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos.

listaInfinitaDePostres :: [Postre]
listaInfinitaDePostres = bizcochoBorracho : listaInfinitaDePostres

magoInfinito :: Mago
magoInfinito = UnMago "Mago infinito" hechizosInfinitos 2

hechizosInfinitos :: [Hechizo]
hechizosInfinitos = cycle [incendio, immobulus]

-- B) Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos 
-- ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar conceptualmente.

-- > losDejaraListos hechizo listaInfinitaDePostres

-- Como la funcion losDejaraListos evalua toda la lista (debido al all), entonces pueden ocurrir dos situaciones:
-- Debido a la evaluacion perezosa (lazy evaluation) que utiliza haskell, entonces si hay algun postre de la lista infinita
-- que NO queda listo luego de realizarle el respectivo hechizo, entonces se detiene el chequeo de la lista (porque ya es 
-- suficente al haber encontrado un valor que no cumple, asi funciona el all). Otro caso seria si efectivamente todos
-- los postres quedaron listos, entonces queda ejectuando/chequendo que cada uno de los postres de la lista infinita cumpla
-- con la condicion (como la lista es infinita entonces nunca para de evaluar)

-- En conclusuion la unica consulta que puedo hacer y me de una respuesta, seria hacer una consulta sobre una lista infinita
-- sabiendo que uno de los postres NO quedase listo, por lo tanto me otorgaria una respuesta False (ya que hay por lo menos}
-- uno que NO cumple, NO cumpliendose el all)

-- C) Suponiendo que un mago tiene infinitos hechizos 
-- ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.

-- > mejorHechizo postre magoInfinito

-- En este caso no va a encontrar NUNCA el mejor hechizo, aunque haskell trabaje con una evaluacion perezosa (lazy evaluation)
-- tendra que evaluar/chequear toda la lista de hechizos para obtener un resultado y como la lista es infinita entonces NUNCA
-- parara de chequear, por lo tanto, nunca terminara de verificar si hay algun mejor hechizo que el siguiente de la lista y asi
-- sucesivamente con el resto de la lista (algun mejor hechizo que otro)