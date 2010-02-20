import Control.Monad.State

type ImpState = Int

threenp1 :: State ImpState Int
threenp1 = do x <- get
	      (if x == 1
	          then return x
	       else if (mod x 2) == 0
	       then 
	       	    do put (div x 2)
	               threenp1
               else 
	      	    do put (3*x+1)
		       threenp1)

threenplus1 x = snd( runState threenp1 x)
