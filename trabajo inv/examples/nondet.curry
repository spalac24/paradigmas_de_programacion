coin = 0 ? 1

double x = x+x


insert e [] = [e]
insert e (x:xs) = (e:x:xs) ? (x : (insert e xs))

perm [] = []
perm (x:xs) = insert x (perm xs)

sorted :: [Integer] -> [Integer]
sorted [] = []
sorted [x] = [x]
sorted (x:y:xs) | ((x < y)::Bool) =:= True = x : (sorted (y:xs))

mySort x = sorted (perm x)

