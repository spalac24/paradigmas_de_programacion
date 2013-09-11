data Nat = Z | S Nat

add:: Nat -> Nat -> Nat

add Z y = y
add (S x) y = S (add x y)

leq:: Nat -> Nat -> Bool

leq Z _ = True
leq (S _) Z = False
leq (S x) (S y) = leq x y
