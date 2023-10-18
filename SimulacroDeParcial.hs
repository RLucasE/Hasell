import Data.Char (isDigit, digitToInt)
-- Usando conceptos de map y filter definir sumDigitFromText :: String ­> Int
-- La cual suma todos los dígitos de una cadena de texto. Ejemplo: sumDigitFromText "En el piso 17
-- venden 2 Cel. a $7599" ­> Resultado: 40

digitFromText :: String -> String
digitFromText = filter isDigit 

pasarListaInt :: String -> [Int]
pasarListaInt = map digitToInt

sumarLista :: [Int] -> Int
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

sumDigitFromText :: String -> Int
sumDigitFromText lis = sumarLista (pasarListaInt (digitFromText lis))

biseccion :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion f a b e 
 | abs (f c) < e = c 
 | f c * f a < 0 = biseccion f a c e
 | f c * f a > 0 = biseccion f c b e
  where 
    c = (a + b) / 2

g :: Int -> Int
g = (*) (-1)

f :: Int -> [Int] -> [Int]
f e l = map (g . (e+)) (filter (5<) l)