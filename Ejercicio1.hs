esNumeroNegativo :: Float -> Bool
esNumeroNegativo x = x < 0

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = (x `mod` y) == 0

fueraRango :: Int -> Int -> Int -> Bool 
fueraRango x y z = y < x && x < z 