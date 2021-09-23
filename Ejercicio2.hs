import Data.Fixed (mod')
{- Crear una lista de valores entre -10 y 10 -}
lista :: [Integer]
lista = [-10..10]

{- Crear una lista de pares entre 80 y 120 -}
listaPar :: [Integer]
listaPar = [80,82..120]

{- Crear una lista de impares entre 71 y 51 en orden inverso -}
listaImpar :: [Integer]
listaImpar = reverse [51,53..71]

-- Listas por comprension y map
{- Crear una lista de multiplos de 4 para menores a 45 -}
listaMultiplo :: [Integer]
listaMultiplo = [ x | x <- [0..45], x `mod` 4 == 0]

{- Crear una lista de potencias de 3 de impares menores a 30
    Resolver por comprension y con la funcion map
 -}
listaPot :: [Integer]
listaPot = [ x | x <- map (^3)[1,3..30], x<30 ]

{- Crear una lista de valores para la funcion x^2 + 1 entre [-10, 10]
    Resolver por comprension y con la funcion map
 -}
lista2 :: [Integer]
lista2 =   [ x+1 | x <- map (^2)[-10..10] ]

-- Listas por comprension y zip
{- Crear una lista de pares ordenados entre 1 y 5
    Resolver por comprension y con la funcion zip
 -}
lista3 :: [(Integer, Integer)]
lista3 =   [ (x,y) | (x,y) <- zip [1..5] [1..5]  ]

-- Listas por comprension y zipWith
{- Crear una lista de valuaciones de la funcion x*2 + y para x en [-2,2] e y en [3,7]
    Resolver por comprension y con la funcion zipWith
 -}
lista4 :: [(Integer)]
lista4 = [(x) | (x) <- zipWith (+) (zipWith (*) [-2..2] [-2..2]) [3..7] ]


{- Crear una lista de valuaciones de la funcion x*y para x en [3,7] e y en [-2,2]
    Resolver por comprension y con la funcion zipWith
 -}
lista5 :: [Integer]
lista5 =  [ (x) | (x) <- zipWith (*) [3..7] [-2..2]]