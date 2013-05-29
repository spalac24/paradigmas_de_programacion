-- Testing functional features:

import Assertion

-- Concatenating two lists:
append :: [a] -> [a] -> [a]
append []     x  = x
append (x:xs) ys = x : append xs ys

-- Reverse (naively) the order of elements in a list:
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]


-- and now the test cases:

test1 = AssertEqual "append" (append [1,2] [3,4]) [1,2,3,4]

test2 = AssertEqual "rev" (rev [1,2,3,4]) [4,3,2,1]
