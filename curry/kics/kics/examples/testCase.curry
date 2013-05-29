-- Testing case expressions:

import Assertion


swap :: [Int] -> [Int]
swap l = case l of
  [x,y] -> [y,x]
  _     -> l

tests1 = AssertEqual "swap1" (swap []     ) []
tests2 = AssertEqual "swap2" (swap [1]    ) [1]
tests3 = AssertEqual "swap3" (swap [1,2]  ) [2,1]
tests4 = AssertEqual "swap4" (swap [1,2,3]) [1,2,3]


f l = case l of
        []   -> 0
        _:xs -> 100 + case xs of
                          [y] -> y
                      + 50

testf1 = AssertEqual  "f1" (f []   ) 0
testf2 = AssertValues "f2" (f [1]) []
testf3 = AssertEqual  "f3" (f [1,2]) 152

g x = case x of
        Just "abc" -> True
        Just "xyz" -> True
        _          -> False

testg1 = AssertEqual "g1" (g (Just "xyz")) True
testg2 = AssertEqual "g2" (g (Just "ab") ) False

h x = case x of
        [1,2] -> True
        _     -> False

testh1 = AssertEqual "h1" (h [1,2]) True
testh2 = AssertEqual "h2" (h [1,3]) False
testh3 = AssertEqual "h3" (h [2,div 1 0]) False

