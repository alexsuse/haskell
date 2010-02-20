import Data.Char

main :: IO ()
main = do x <- getLine
	  let fac = factorList $ rIntList x
	  putStrLn $ show fac

rInt :: String -> Int
rInt = read

rIntList :: String -> [Int]
rIntList = read

factors :: Int -> [Int]
factors x = [p | p <- [1..x], mod x p == 0]

factorList :: [Int] -> [[Int]]
factorList = map factors

anegate :: [Int] -> [Int]
anegate [] = []
anegate (x:xs) = (-x):(anegate xs)

negateList :: [[Int]] -> [[Int]]
negateList = map anegate

takeint :: Int -> [a] -> [a]
takeint 1 (x:xs) = [x]
takeint n (x:xs) = x:(takeint (n-1) xs)

dropint :: Int -> [a] -> [a]
dropint 0 (x:xs) = x:xs 
dropint n (x:xs) = dropint (n-1) xs

sumInt :: [Integer]-> Integer
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs

scansum :: [Integer] -> [Integer]
scansum [] = []
scansum x = [sumInt $ takeint i x| i<-[1..(length x)]]

for:: a -> (a->Bool) -> (a->a) -> (a-> IO ()) -> IO ()
for i p f job = if (p i)
			then 
				do job i
				   for (f i) p f job
		 	 else return ()

charIncrement :: Char -> Char
charIncrement = (chr.(+1).ord)

charGreaterEqual :: Char -> Char -> Bool
charGreaterEqual a b = (ord a) >= (ord b)
