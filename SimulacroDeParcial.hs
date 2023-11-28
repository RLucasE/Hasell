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

intercalar :: Eq a => a -> [a] -> [a]
intercalar _ [] = []
intercalar valor (x:xs) = [x] ++ [valor] ++ intercalar valor xs

acoplarCon :: (a -> a -> a) -> [a] -> [a] -> [a]
acoplarCon _ [] _ = []
acoplarCon f (x1:xs1) (x2:xs2) = f x1 x2 : acoplarCon f xs1 xs2

codCesar :: Int -> String -> String
codCesar n (x:xs) = correrLetra n ([x..'z']++['a'..x]) : codCesar n xs

correrLetra :: Int -> String -> Char
correrLetra _ [] = ' '
correrLetra n (x:xs)
 |n== 0 = x
 |n > 0 = correrLetra (n-1) xs

  
