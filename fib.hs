main = putStrLn "Hell Yeah!"

mostra :: Integer->[Integer]->Integer
mostra 0 _ = 0
mostra 1 (x:xs) = x
mostra n (x:xs) = mostra (n-1) xs


fib = 1:1:[a+b|(a,b)<- zip fib (tail fib)]
