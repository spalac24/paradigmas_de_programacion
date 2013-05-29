map _ [] = []
map f (x:xs) = (f x):(map f xs)

min3 = (\x -> 3-x)