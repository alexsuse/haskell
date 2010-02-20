newtype StateTrans s a = ST( s -> (s, a) )

instance Monad (StateTrans s)
  where
      -- (>>=) :: StateTrans s a -> (a -> StateTrans s b) -> StateTrans s b
          (ST p) >>= k  =  ST( \s0 -> let (s1, a) = p s0
	                                  (ST q) = k a
                                      in q s1 )
      -- return :: a -> StateTrans s a
          return a = ST( \s -> (s, a) )
applyST :: StateTrans s a -> s -> (s, a)
applyST (ST p) s = p s

type ImpState = Int

get :: StateTrans ImpState Int
get = ST(\x-> (x, x))

put :: Int -> StateTrans ImpState ()
put x' = ST(\x->(x',()))

threenp1 :: StateTrans ImpState Int
threenp1 = do x <- get
	      (if x == 1
	       then 
	       	   return 1
	       else if mod x 2 == 0
	       then 
	        	do put $ x `div` 2
		           threenp1
	       else 
	         	do put $ 3*x+1
	                   threenp1)

threenplus1 x = snd (applyST threenp1 x)
