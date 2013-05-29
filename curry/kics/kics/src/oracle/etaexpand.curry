test1 = f (and True)
test2 = f (True&&)

and x = (x&&)

f g = g True && g False

test3 = bar (foo [True])

foo (x:_) = (x&&)

bar g = g True && g True