{- 
Escriba una funcion recursiva que calcule el cociente de la
division entera utilizando solamente sumas y restas.
 -}

cociente :: Integer -> Integer -> Integer
cociente x y
  | x < y = 0
  | otherwise = 1 + cociente (x-y) y

{-
Escriba una funcion recursiva que calcule el producto de los
enteros entre a y b:
   prodAB a b = a * (a+1) * (a+2) * ... * (b-1) * b
 -}

producto :: Integer -> Integer -> Integer
producto a b
    | a < b = a * producto(a+1) b
    | a == b = b
    | otherwise = error "Primer numero mayor"

{- 
Escriba una funcion que calcule la cantidad de cifras de un
entero:
   cantCifras 123 = 3
 -}

numeroCifras :: Integer -> Integer
numeroCifras x
    | x < 10 = 1
    | otherwise = 1 +numeroCifras(div x 10)

{- 
Dada la representacion de fechas como tupla (day, month, year),
definir la funcion showFecha :: (Int, Int, Int) -> [Char].
Ejemplos: 
   showFecha (10, 12, 2013) = "10 de Diciembre de 2013"
   showFecha (21,  0, 2021) = "21 de Enero de 2021"
Recordar que Int es de la clase Show, y se puede usar la
instruccion show para convertir a string.
 -}
 
meses :: [[Char]]
meses = ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"]


showFechas :: (Int, Int, Int) -> [Char]
showFechas (a,b,c)
  | a < 1 || b < 1  || c < 1 = "Error, fecha mal ingresada"
  | a <= 31 && b <= 12  = show a ++ " de "++ meses !! (b-1) ++ " de " ++ show c
  | otherwise = error "Error, se cargo mal"

