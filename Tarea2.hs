module Tarea2 where

type Vertice = Int
type Peso = Int
type VerticeConPeso = (Vertice,Peso)

type GrafoP= [(Vertice,[VerticeConPeso])]

type Etiquetas = [(Vertice, Peso)]
type Visitados = [Vertice]

inf = maxBound::Int

-- Ejemplo de la letra
ejemplo1 :: GrafoP
ejemplo1 = [(1, [(2,2), (3,1)]), (2, [(4,1)]), (3, [(4,3), (5,4)]), (4, [(6,2)]), (5, [(6,2)]), (6, [])]

-- Otro ejemplo
ejemplo2 :: GrafoP
ejemplo2 = [(1, [(2,3), (4,5), (3,1), (5,0)]), (2, [(4,1), (3,1), (1,1)]), (3, [(4,2)]), (4, []), (5, [(4,1)])]

-- ******************************************* FUNCIONES A IMPLEMENTAR *******************************************

-- Ejercicio 1 
-- grafo = [(Vertice,[VerticeConPeso])]
vertices::GrafoP->[Vertice]
vertices [] = []
vertices ( (vertice,l):ys ) = vertice:vertices ys;

-- Ejercicio 2
crearListaEtiquetasSinPeso :: [Vertice] -> Etiquetas
crearListaEtiquetasSinPeso  = undefined

-- Ejercicio 3
actualizarPeso :: Etiquetas -> Vertice -> Peso -> Etiquetas
actualizarPeso = undefined

-- Ejercicio 4
costoAsignacion :: Etiquetas -> Vertice -> Int
costoAsignacion es v = undefined

-- Ejercicio 5
verticeConAdyacentes :: GrafoP -> Vertice -> (Vertice,[VerticeConPeso])
verticeConAdyacentes gs v = undefined

-- Ejercicio 6
-- Sugerencia: Se pueden definir funciones auxiliares para facilitar la tarea
actualizarAsignacion :: (Vertice, [VerticeConPeso]) ->  Etiquetas -> Etiquetas
actualizarAsignacion = undefined

-- Ejercicio 7
pesoDeUnVertice :: [VerticeConPeso] -> Vertice -> Int
pesoDeUnVertice vps v = undefined

-- Ejercicio 8
perteneceVertice :: [VerticeConPeso] -> Vertice -> Bool
perteneceVertice = undefined

-- Ejercicio 9
verticeMenorCostoNoVisitado :: Etiquetas -> Visitados -> Vertice
verticeMenorCostoNoVisitado = undefined

-- ************************************************** FIN TAREA **************************************************

cmc :: GrafoP -> Visitados -> Etiquetas -> Vertice -> Vertice -> Peso
cmc g vs es actual destino
    | length (vertices g) == (length vs) = costoAsignacion es destino
    | otherwise = cmc g (actual:vs) (asignacionActualizada) (verticeMenorCostoNoVisitado asignacionActualizada (actual:vs)) destino
        where asignacionActualizada = actualizarAsignacion (verticeConAdyacentes g actual) es

menorCosto :: GrafoP -> Vertice -> Vertice -> Peso
menorCosto g origen destino = cmc g [] (actualizarPeso (crearListaEtiquetasSinPeso (vertices g)) origen 0) origen destino

-- Pruebas de los ejemplos
pruebaEjemplo1 :: Int
pruebaEjemplo1 = menorCosto ejemplo1 1 6 -- resultado deberia ser 5.

pruebaEjemplo2 :: Int
pruebaEjemplo2 = menorCosto ejemplo2 1 4 -- resultado deberia ser 1.
