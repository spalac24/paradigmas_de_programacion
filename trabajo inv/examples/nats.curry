data Nat = Z | S Nat


add         :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)


sub x y | add y z =:= x  = z where z free


leq Z     _     = True
leq (S _) Z     = False
leq (S x) (S y) = leq x y