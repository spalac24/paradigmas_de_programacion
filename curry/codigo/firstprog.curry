nine = 9
square x = x*x

loop = 1+loop

fac :: Int -> Int
fac n = if n == 0 then 1 else n* (fac (n-1))


True :-: False = True
False :-: True = True
False :-: False = False
True :-: True = False