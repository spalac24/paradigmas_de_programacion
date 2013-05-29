--from http://www.mail-archive.com/haskell-cafe@haskell.org/msg63957.html

numDigits1 :: Integer -> Integer
numDigits1 n = numDigits1_aux 10 (abs(n)) where
           numDigits1_aux base n = 1 + fst (ilog base n) where
                      ilog base  n
                           | n < base     = (0, n)
                           | otherwise = let (e, r) = ilog (base*base) n
                                       in  if r < base then (2*e, r) else (2*e+1, r `div` base)





--own_naive (SLOW)
numDigits_naive :: Integer-> Integer
numDigits_naive n
           | n < 10 = 1
           | otherwise = 1+ numDigits_naive(n `div` 10)

























--own_tail_recursive(still slow)
numDigits :: Integer -> Int
numDigits n = numDigits_aux (n,10) where
          numDigits_aux (a,b) 
                        |a < b = 0
                        |otherwise = let r = numDigits_aux(a,b*b) in
                                   if b^(2*r) < a then 2*r+1 else 2*r

main::IO()
main = do
     g <- getLine
     let (a:b:_) = (map read $ words(g))::[Integer]
     putStrLn . show $ numDigits (a^b)