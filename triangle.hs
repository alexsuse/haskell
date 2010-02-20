main :: IO ()
main = do
	putStrLn "The base?"
	basestrn <- getLine
	let base = rDouble basestrn
	putStrLn "The height?"
	heightstrn <- getLine
	let height = rDouble heightstrn
	putStrLn $ "The area of the triangle is " ++ show (area height base)

altmain :: IO ()
altmain = putStrLn "The base?" >>
	getLine >>= f
	where
	f first = putStrLn "The hegight?" >>
		getLine >>= g
		where
		g second = putStrLn $ "The area of the triangle is " ++ show (area (rDouble first) (rDouble second))


rDouble :: String -> Double
rDouble = read

area :: Fractional a => a -> a -> a
area base height = 0.5*base*height
