main :: IO ()
main = do c <- getline
	  putstr c

getline :: IO String
getline = do c <- getChar
             if c == '\n'
	          then return " "
		  else do l <- getline
		  	  return (c:l)

putstr :: String -> IO ()
putstr s = sequence_ (map putChar s)
