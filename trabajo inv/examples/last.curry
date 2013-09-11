last :: [a] -> a
last x
     | _++[e] =:= x = e
     where e free
