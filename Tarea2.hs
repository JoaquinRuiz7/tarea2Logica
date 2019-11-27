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
-- Falta ver como poner infinito
crearListaEtiquetasSinPeso :: [Vertice] -> Etiquetas
crearListaEtiquetasSinPeso  [] = []
crearListaEtiquetasSinPeso (x:xs) = (x,0):crearListaEtiquetasSinPeso xs; 

-- Ejercicio 3
actualizarPeso :: Etiquetas -> Vertice -> Peso -> Etiquetas
actualizarPeso [] v p = []
actualizarPeso ((v,peso):xs) vertice p 
    | v == vertice && peso > p = (v,p):actualizarPeso xs vertice p -- si encontre agrego la etiqueta nueva
    | otherwise = (v,peso):actualizarPeso xs vertice p  -- si no encontre agregotodos al retorno de la funcion hasta que encuentre.

-- Ejercicio 4
--que dada la lista de etiquetas y un vertice, retorna el costo
--guardado en la lista para dicho vertice.
costoAsignacion :: Etiquetas -> Vertice -> Int
costoAsignacion [] v  = 0 -- si no encuentra el vertice da error
costoAsignacion ( (v,peso):xs ) vt 
    | v == vt = peso
    | otherwise = costoAsignacion xs vt;

-- Ejercicio 5
--que dado un grafo g y un
--vertice v, retorna el par ordenado que contiene a v y la lista de v´ertices adyacentes a v con sus respectivos pesos.
verticeConAdyacentes :: GrafoP -> Vertice -> (Vertice,[VerticeConPeso])
verticeConAdyacentes [] v = error "Empty graph or item not in graph"
verticeConAdyacentes ( ( v,l ):xs ) vertice 
    | v == vertice =  ( v,l ) 
    | otherwise = verticeConAdyacentes xs vertice;

-- Ejercicio 6
-- Sugerencia: Se pueden definir funciones auxiliares para facilitar la tarea
actualizarAsignacion :: (Vertice, [VerticeConPeso]) ->  Etiquetas -> Etiquetas
actualizarAsignacion = undefined

-- Ejercicio 7
pesoDeUnVertice :: [VerticeConPeso] -> Vertice -> Int
pesoDeUnVertice [] v = 0
pesoDeUnVertice ( (v,p):xs ) vertice
    | v == vertice = p 
    | otherwise = pesoDeUnVertice xs vertice;


-- Ejercicio 8
perteneceVertice :: [VerticeConPeso] -> Vertice -> Bool
perteneceVertice [] v  = False
perteneceVertice ( ( v,p ):xs ) vertice
    | v == vertice = True
    | otherwise = perteneceVertice xs vertice;

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
