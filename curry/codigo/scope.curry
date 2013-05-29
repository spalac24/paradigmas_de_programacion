f _ [] = False
f x (x:_) = True
f x (_:ys) = f x ys

pow n m = if m == 1 then n else n*(pow n (m-1))

fact n = fact_aux 1 n where
  fact_aux n m = if m == 1 then
                   n
                 else
                   fact_aux (m*n) (m-1)
                   
fact_slow n = if n == 0 then 1 else n*fact_slow(n-1)


--zips a list with elem+1: [1,2,..] => [(1,2), (2,3)..]

zipp1 l = zip l (map f l)
          where f x = g x
                g x = x+1
                
zipp2 l = let
    f x = g x; g x = x+2
  in
   zip l (map f l)