import Control.Monad.State

type ImpState = [Int]

threenp1 :: State ImpState Int
threenp1 = do g =<< return.last =<< get
    where g x | x == 1            = return x
              | x `mod` 2 == 0    = do modify $ flip (++) [x `div` 2]
                                       threenp1
              | otherwise         = do modify $ flip (++) [3*x + 1]
                                       threenp1
    
threenplus1 x = snd $ runState threenp1 x

alength :: ImpState-> Int
alength (x:xs) = 1 + alength xs
alength [] = 0