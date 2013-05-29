module ArrayB where 

import GHC.Exts hiding (split)

import Maybe
-- Implementation of Arrays with Braun Trees

infixl 9  ! 

data ArrayB b = Entry (Maybe b) (ArrayB b) (ArrayB b)
	      | Empty
  deriving Eq

emptyArrayB :: ArrayB b
emptyArrayB = Entry Nothing emptyArrayB emptyArrayB

emptyArrayB' :: Word# -> Word# -> ArrayB b
emptyArrayB' b o =
  Entry Nothing 
	(emptyArrayB' (timesWord# (int2Word# 2#) b) o)
        (emptyArrayB' (timesWord# (int2Word# 2#) b) (plusWord# o b))

update :: ArrayB b -> Word# -> b -> ArrayB b
update (Entry v al ar) n v'
  | eqWord# n (int2Word# 0#) = Entry (Just v') al ar
  | otherwise    = 
      let r = and# n (int2Word# (1#)) in
        if eqWord# (int2Word# 0#) r
          then Entry v al (update ar (minusWord# (div2 n) (int2Word# 1#)) v')
          else Entry v (update al (div2 n) v') ar

div2 :: Word# -> Word#
div2 n = shiftRL# n 1#

(!) :: ArrayB b -> Word# -> (Maybe b)
(Entry v al ar) !n 
  | eqWord# n (int2Word# 0#) = v
  | otherwise = let r = and# n (int2Word# (1#)) in
        if eqWord# (int2Word# 0#) r
          then ar!(minusWord# (div2 n) (int2Word# 1#))
          else al!div2 n
Empty ! n = Nothing

split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xys) = let (xs,ys) = split xys in
                    (x:xs,y:ys)

listToArrayB :: [b] -> ArrayB b
listToArrayB xs = listToArrayB' (int2Word# 1#) (int2Word# 0#) xs

listToArrayB' :: Word# -> Word# -> [b] -> ArrayB b
listToArrayB' b o [] = Empty -- emptyArrayB' b o
listToArrayB' b o (x:xs) = let (ys,zs) = split xs in
                         Entry (Just x) (listToArrayB' (timesWord# (int2Word# 2#) b) o ys)
                                        (listToArrayB' (timesWord# (int2Word# 2#) b) (plusWord# o b) zs)

rows k [] = []
rows k xs = let (ys,zs) = splitAt k xs in
              (k,ys):rows (2*k) zs

built (k,xs) ts = zipWith3 Entry xs ts1 ts2
  where (ts1,ts2) = splitAt k (ts++repeat Empty)

makeArray = head . foldr built [] . rows 1

