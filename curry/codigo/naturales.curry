data Natural = Zero | Succ Natural
data Tree a = Leaf | Node a (Tree a) (Tree a)