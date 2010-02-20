main = do putStrLn "Hell Yeah!!"

iter :: (Double,Double) -> (Double,Double)-> (Double,Double)
iter (a,b) (c,d) = soma (c,d) (square (a,b))


square :: (Double,Double) -> (Double,Double)
square (a,b) = (a*a - b*b,a*b + b*a)

soma :: (Double,Double) -> (Double,Double)->(Double,Double)
soma (a,b) (c,d) = (a + c, b + d)

seq :: (Double,Double)-> (Double,Double)->[(Double,Double)]
seq (a,b) (c,d) = (a,b) : [soma (c,d) (square (m,n))| (m,n) <- seq (a,b) (c,d) ]

mandel :: (Double,Double) ->(Double,Double)->Integer ->Bool
mandel (a,b) (c,d) n = [(k,j) | (k,j) <- seq (a,b) (c,d) , k*k+j*j>1.0] == []  


