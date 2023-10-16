ptoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
ptoMedio (x1,y1) (x2,y2) = ((x1 + x2)/2,(y1 + y2)/2) 

normaEuclidea :: (Float,Float) -> Float
normaEuclidea (x,y) =  sqrt(x^2 + y^2)

segundos2Tiempo :: Int -> (Int,Int,Int)
segundos2Tiempo segundos = (hora,minutos,segundos)
    where 
     minutos = segundos `div` 60
     hora = minutos `div` 60
        