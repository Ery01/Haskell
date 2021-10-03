import Data.Fixed (mod')

{- Fechas
   -----------
   Ejercicio 7.9.1. Definir el tipo de datos Mes para representar los doce meses y
   hacerlo instancia de Eq y Show.


   Solución: -}
data Mes = Enero | Febrero | Marzo | Abril | Mayo | Junio | Julio | Agosto | Septiembre | Octubre | Noviembre | Diciembre deriving (Show, Eq, Ord)

{- Ejercicio 7.9.2. Definir la función
   divisible :: Int -> Int -> Bool
   tal que (divisible x y) se verifica si x es divisible por y. Por ejemplo,
   divisible 9 3 ; True
   divisible 9 2 ; False

   Solución: -}
divisible :: Int -> Int -> Bool
divisible x y = mod x y == 0    

{- Ejercicio 7.9.3. La definición de año bisiesto es 
   . un año divisible por 4 es un año bisiesto (por ejemplo 2008); 
   . excepción: si es divisible por 100, entonces no es un año bisiesto 
   . excepción de la excepción: si es divisible por 400, entonces es un año 
     bisiesto (por ejemplo 2000).
	 
   Definir la función
   bisiesto :: Int -> Bool
   tal que (bisiesto a) se verifica si el año a es bisiesto. Por ejemplo,
   bisiesto 2008 ; True
   bisiesto 1900 ; False
   bisiesto 2000 ; True
   bisiesto 2007 ; False

   Solución: -}
esBisiesto :: Integer -> Bool
esBisiesto x 
    |(mod x 4) == 0 = if (((mod x 100) == 0) || ((mod x 400) == 0)) then False else True
    |otherwise = False

bisiesto :: Int -> Bool
bisiesto x = divisible x 400 || (divisible x 4 && not (divisible x 100))    

{- Ejercicio 7.9.4. Definir la función
   diasDelMes :: Mes -> Int -> Int
   tal que (diasDelMes m a) es el número de días del mes m del año a. Por ejemplo,
   diasDelMes Febrero 2008 ; 29
   diasDelMes Febrero 2007 ; 28

   Solución: -}
diasDelMes :: Mes -> Int -> Int
diasDelMes m a 
      | m == Enero || m == Marzo || m == Mayo || m == Julio || m == Agosto || m == Octubre || m == Diciembre = 31
      | m == Febrero = 28
      | bisiesto a && m == Febrero = 29
      | otherwise = 30   

{- Ejercicio 7.9.5. Definir el tipo Fecha para representar las fechas mediante el 
  día, el mes y el año. Por ejemplo,
  Main> :t F 3 Enero 2000
  F 3 Enero 2000 :: Fecha
  Main> :i Fecha
  -- type constructor
  data Fecha
  
  -- constructors:
  F :: Int -> Mes -> Int -> Fecha
  -- selectors:
  dia :: Fecha -> Int
  mes :: Fecha -> Mes
  agno :: Fecha -> Int

  Solución: -}
data Fecha = F {dia :: Int, mes :: Mes, agno :: Int} deriving (Show, Eq, Ord)

{- Ejercicio 7.9.6. Definir la función
   fechaValida :: Fecha -> Bool
   tal que (fechaValida f) se verifica si f es una fecha válida. Por ejemplo,
   fechaValida (F 29 Febrero 2008) ; True
   fechaValida (F 0 Febrero 2008) ; False
   fechaValida (F 29 Febrero 2007) ; False

   Solución: -}
fechaValida :: Fecha -> Bool
fechaValida f = agno f > 0 && dia f > 0 && dia f <= diasDelMes (mes f)(agno f)   