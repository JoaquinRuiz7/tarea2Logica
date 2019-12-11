module Dijkstra where
-- Estudiante : Joaquin Ruiz
-- Nro : 206164

type Vertice = Int
type Peso = Int
type VerticeConPeso = (Vertice,Peso)

type GrafoP= [(Vertice,[VerticeConPeso])]

type Etiquetas = [(Vertice, Peso)]
type Visitados = [Vertice]

inf = maxBound::Int

etiquetas::Etiquetas
etiquetas = [(1,3),(2,4),(3,5),(4,11)]
visitdados::Visitados
visitdados = [2,3]
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
crearListaEtiquetasSinPeso  [] = []
crearListaEtiquetasSinPeso (x:xs) = (x,inf):crearListaEtiquetasSinPeso xs; 

-- Ejercicio 3
actualizarPeso :: Etiquetas -> Vertice -> Peso -> Etiquetas
actualizarPeso [] v p = []
actualizarPeso ((v,peso):xs) vertice p 
    | v == vertice && peso > p = (v,p):actualizarPeso xs vertice p 
    | otherwise = (v,peso):actualizarPeso xs vertice p  

-- Ejercicio 4
--que dada la lista de etiquetas y un vertice, retorna el costo
--guardado en la lista para dicho vertice.
costoAsignacion :: Etiquetas -> Vertice -> Int
costoAsignacion [] v  = 0
costoAsignacion ( (v,peso):xs ) vt 
    | v == vt = peso
    | otherwise = costoAsignacion xs vt;

-- Ejercicio 5
--que dado un grafo g y un
--vertice v, retorna el par ordenado que contiene a v y la lista de vertices adyacentes a v con sus respectivos pesos.
verticeConAdyacentes :: GrafoP -> Vertice -> (Vertice,[VerticeConPeso])
verticeConAdyacentes [] v = (v,[])
verticeConAdyacentes ( ( v,l ):xs ) vertice 
    | v == vertice =  ( v,l ) 
    | otherwise = verticeConAdyacentes xs vertice;

actualizarAsignacionAux::(Vertice,[VerticeConPeso])-> Etiquetas ->Etiquetas -> Etiquetas
actualizarAsignacionAux (v,l) [] [] = l 
actualizarAsignacionAux (v,[]) e e2 = e2;
actualizarAsignacionAux (v,(v1,p):xs) e ( (v2,p2):ys )
    | ( (costoAsignacion e v) + p ) < p2 = (v1,( costoAsignacion e v )+p ):actualizarAsignacionAux (v,xs) e ys
    | otherwise = (v1,p):actualizarAsignacionAux (v,xs) e ys;
-- Ejercicio 6
-- Sugerencia: Se pueden definir funciones auxiliares para facilitar la tarea
actualizarAsignacion :: (Vertice, [VerticeConPeso]) ->  Etiquetas -> Etiquetas
actualizarAsignacion p e = actualizarAsignacionAux p e e;
{-- 
actualizarAsignacion (v, l) [] = l
actualizarAsignacion (v,[]) e = e  
actualizarAsignacion (v,( (v1,p):xs ) )  ( (v2,p2):ys )
    |   ( (costoAsignacion ( (v2,p2):ys ) v ) + p ) < p2 = (v1, (costoAsignacion ( (v2,p2):ys ) v) + p):actualizarAsignacion (v,xs) ( (v2,p2):ys ) 
    |   otherwise =  (v1,p):actualizarAsignacion (v,xs) ( (v2,p2):ys );
--}

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


noVisitados::Etiquetas->Visitados->Etiquetas
noVisitados [] _ = []
noVisitados l [] = l 
noVisitados ((v,p):xs) l 
    | elem v l = noVisitados xs l 
    | not ( elem v l ) = (v,p):noVisitados xs l; 

minimoCosto::Etiquetas->Vertice
minimoCosto [(v,p)] = p
minimoCosto ( (v,p):xs )
    | p < minimoCosto xs = p
    | otherwise = minimoCosto xs;

verticeMinimoCosto::Etiquetas->Int->Vertice
verticeMinimoCosto [] min = error "No hay etiquetas";
verticeMinimoCosto ( (v,p):xs ) min 
    | p == min = v
    | otherwise = verticeMinimoCosto xs min;
-- Ejercicio 9
--que dada la lista de etiquetas retorna
--el vertice de menor costo que no haya sido visitado.

verticeMenorCostoNoVisitado :: Etiquetas -> Visitados -> Vertice
verticeMenorCostoNoVisitado  e v = verticeMinimoCosto e ( minimoCosto (noVisitados e v ))

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
