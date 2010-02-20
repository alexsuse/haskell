main :: IO ()
main = mygetline >>= (\c -> if palindrome c
								then myputstr "This is a Palindrome!"
								else myputstr "This is not a Palindrome!")

mygetline :: IO String
mygetline = getChar >>= \c -> if c == '\n'
	                    then return " "
		            else mygetline >>= \l -> return (c:l)

myputstr :: String -> IO ()
myputstr s = sequence_ (map putChar s)

invertstring :: IO String
invertstring = mygetline >>= \s -> return (reversestring s)

reversestring :: String -> String
reversestring [] = []
reversestring (x:xs) = reversestring xs ++ [x] 

strcmp :: String -> String -> Bool
strcmp [] [] = True
strcmp (x:xs) (y:ys) = if x /= y 
						then False
						else strcmp xs ys
						
palindrome :: String -> Bool
palindrome s = strcmp (killspaces s) (reversestring (killspaces s))

killspaces :: String -> String
killspaces [] = []
killspaces (x:xs) = if x /= ' '
					then x:killspaces xs
					else killspaces xs