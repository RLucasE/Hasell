quitarCaracter :: Char -> String -> String
quitarCaracter _ [] = []
quitarCaracter a (x:xs) 
 | a == x = quitarCaracter a xs
 | otherwise = x : quitarCaracter a xs

--Limpia los caracteres de la segunda cadena que aparezcan en la pñrimera
limpiar :: String -> String -> String
limpiar [] cadena = cadena
limpiar (x1:xs1) lis = limpiar xs1 lis2
  where lis2 = quitarCaracter x1 lis

calcularPromedio :: [Float] -> Float
calcularPromedio [] = 0
calcularPromedio (x:xs) = sum xs / fromIntegral (length xs)

restarNumero :: [Float] -> Float -> [Float]
restarNumero [] _ = []
restarNumero (x:xs) numero = x - numero : restarNumero xs numero 

difPromedio :: [Float] -> [Float]
difPromedio lista = restarNumero lista (calcularPromedio lista)

listaIgualA :: [Integer] -> Integer -> Bool
listaIgualA [] _ = True
listaIgualA (x:xs) valor
 |x == valor = listaIgualA xs x
 |x /= valor = False

todosIguales :: [Integer] -> Bool 
todosIguales (x:xs) = listaIgualA xs x
