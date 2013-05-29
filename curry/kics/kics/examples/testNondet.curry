-- Testing non-deterministic features:

import Assertion

coin = 0 ? 1

testCoin = AssertValues "coin" coin [1,0]


double x = x+x

testCoinCoin = AssertValues "coin+coin" (coin+coin) [0,1,1,2]

testDouble = AssertValues "double" (double coin) [0,2]


-- Non-deterministic permutation sort:
insert x []     = [x]
insert x (y:ys) = (x:y:ys) ? (y:insert x ys)

permut []     = []
permut (x:xs) = insert x (permut xs)

sorted []  = True
sorted [_] = True
sorted (x:y:ys) | x<=y = sorted (y:ys)

psort xs | sorted ys = ys   where ys = permut xs

testSort = AssertValues "permsort" (psort [4,1,3,2]) [[1,2,3,4]]
