data Nat = Z | S Nat

add x Z = x
add x (S y) = S (add x y)

leq Z _ = True
leq (S _) Z = False
leq (S x) (S y) = leq x y