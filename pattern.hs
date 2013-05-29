ecua:: Float -> Float -> Float -> (Float,Float)
ecua a b c = (x1,x2) 
  where
    det = sqrt(b*b-4*a*c)
    x1 = (-b + det)/(2*a)
    x2 = (-b - det)/(2*a)

f x@(y:xs) = x

fact m@(x:xs) = 