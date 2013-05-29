-- Testing higher-order features:

import Assertion

-- apply a function to all list elements (predefined as `map'):
map_ ::  (a->b) -> [a] -> [b]
map_ _ []      = []
map_ f (x:xs)  = f x : map_ f xs

-- accumulate all list elements (predefined as `foldr'):
foldr_ ::  (a->b->b) -> b -> [a] -> b
foldr_ _ z []     = z
foldr_ f z (h:t)  = f h (foldr_ f z t)

-- increment function:
inc x = x+1

-- and now the test cases:

test1 = AssertEqual "map inc" (map inc [0,2,1]) [1,3,2]
test2 = AssertEqual "map (+ 1)" (map (+ 1) [0,2,1]) [1,3,2]
test3 = AssertEqual "foldr (sum)" (foldr (+) 0 [1,0,2]) 3
test4 = AssertEqual "foldr (prod)" (foldr (*) 1 [1,2,3,4,5]) 120
test5 = AssertEqual "foldr (prod+lambda)"
                    (foldr (\ x y -> x * y) 1 [1,2,3,4,5]) 120
