test1 = foo (const$True)

foo g = null [g True] || g True


test2 = null (cons true nil)

true = True
nil = []
cons = (:)


test3 = (id . id) 0

test4 = foldr (.) (const true) [const true] true

data Fun a b = Fun (a -> b)

app (Fun f) = f

test5 = app (Fun id) 42

test6 = bar [id,not]

bar fs = foldr1 (.) fs True

foo3 :: (Int->Int->Int) -> (Int,[a]) -> [(Int->Int,[a])] -> [(Int->Int,[a])]
foo3 v1 (v4,v5) v3 = (v1 v4,v5) : v3

foo2 :: (Int->Int->Int,[a]) -> [(Int->Int,[a])] -> [(Int->Int,[a])]
foo2 (v3,v4) v2 = foldr (foo3 v3) [] [(42,v4)] ++ v2


foo1 :: [a] -> [(Int -> Int,[a])]
foo1 v1 = foldr foo2 [] [((+),v1)]

test7 :: [(Int,[a])]
test7 = map (\ (f,l) -> (f 42,l)) (foo1 [])

part x = aux x

aux x y = (x,y)

tupl f g x = (f x,g x)

test8 = apply (tupl (tupl (part 1) (part 2)) (part 3)) 4

