myTake :: Int -> [a] -> [a]

myTake 0 _ = []
myTake n (x:xs) = x:(myTake (n-1) xs)

--myTake 3 (x++y) =:= [1,2,3] where x,y free
