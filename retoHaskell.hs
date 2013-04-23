numDigs :: Integer -> Integer
numDigs n 
  | abs(n) < 10 = 1
  | otherwise = 1 + (numDigs (n`div` 10))