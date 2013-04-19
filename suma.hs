data Nat = Zero | Suc Nat deriving (Show)

add :: Nat -> Nat -> Nat
add x Zero = x
add x (Suc y) = Suc (add x y)

mul :: Nat -> Nat -> Nat
mul _ Zero = Zero
mul x (Suc y) = add (mul x y) x

main :: IO ()
main = do
  print $ show Zero
  print $ show (Suc (Suc Zero))
