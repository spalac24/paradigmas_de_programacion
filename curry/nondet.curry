choose x _ = x
choose _ y = y

one23 = choose 1 (choose 2 3)



last x | l++[e] =:= x = e where l,e free