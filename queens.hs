main = queens 20

queens n      =  place n [] [] [1 .. n]

place c d1 d2 rs
   | c == 0    =  [[]]
   | otherwise =  [ q:qs
                  | (q, rs')<-remove rs,
                    (q-c) `notElem` d1,
                    (q+c) `notElem` d2,
                    qs<-place (c-1) ((q-c):d1) ((q+c):d2) rs']

perms	:: [a] -> [[a]]
perms [] = [[]]
perms x = [a:z| (a,y)<-remove x, z<-perms y]

remove 	:: [a] -> [(a,[a])]
remove [] = []
remove (a:x) = (a,x) : [(b,a:y)| (b,y)<-remove x]
