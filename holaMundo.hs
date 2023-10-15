import Data.Char
-- fac_P 0 = 1
-- fac_P x = x * fac_P(x - 1)

fueraRango :: Int -> Int -> Int -> Bool 
fueraRango x y z = y < x && x < z    

esVocal :: Char -> Bool
esVocal a = (toLower a) `elem` "aeiou" 
    
-- contarVocales :: String -> (Int,Int) 
-- contarVocales [] = 0
-- contarVocales (x:xs)
--  | esVocal x = 1 + contarVocales xs
--  | 
--  | otherwise = contarVocales xs

mulList :: Float -> [Float] -> [Float]
mulList _ [] = []
mulList a (x:xs) = a * x : mulList a xs

escalarXMatriz :: Float -> [[Float]] -> [[Float]]
escalarXMatriz _ [] = []
escalarXMatriz a (x:xs)= mulList a x : escalarXMatriz a xs

-- sumaVectores :: [Float] -> [Float] -> [Float]
-- sumaVectores _ [] = []
-- sumaVectores (x1,xs1) (x2,xs2) = x1 + x2 : sumaVectores xs1 xs2



-- multEscalar :: Float -> [[Float]]