import Control.Monad.State

type ImpState = Int

threenp1 :: State ImpState Int
threenp1 = do x <- get
	   (if x == 1
	    then
	   	 return x
	    else if (mod x 2) == 0
	    then
	         do put $ x `div` 2
		    gcdST
            else
	         do put $ 3*x+1
		    gcdST)

threenplus1 x = snd( runstate threenp1 x)
