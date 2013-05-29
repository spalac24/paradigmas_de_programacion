module IArray 
  (array, (!), listArray, bounds, assocs, (//), amap, ixmap
   {-,accumArray, accum-}) where

data Array _

array ::  (Int, Int) -> [(Int, e)] -> Array e
array external

(!) ::  Array e -> Int -> e
(!) external


listArray ::  (Int, Int) -> [e] -> Array e
listArray bs@(minI,maxI) es = array bs (zip [minI .. maxI] es)

bounds ::  Array e -> (Int, Int)
bounds external

assocs ::  Array e -> [(Int, e)]
assocs external

indices ::  Array e -> [Int]
indices = map fst . assocs

elems ::  Array e -> [e]
elems = map snd . assocs

(//) ::  Array e -> [(Int, e)] -> Array e
(//) external

amap :: (a->b) -> Array a -> Array b
amap f a = array (bounds a) (map (\ (i,e) -> (i,f e)) (assocs a))

ixmap :: (Int -> Int) -> Array a -> Array a
ixmap f a = let newAssocs = map (\ (i,e) -> (f i,e)) (assocs a)
                newBounds = minMax (map fst newAssocs)
             in array newBounds newAssocs
  where
    minMax [] = (0,0)
    minMax (x:xs) = miMa x x xs

    miMa mi ma [] = (mi,ma)
    miMa mi ma (x:xs) = miMa (if x<mi then x else mi) 
                             (if x>ma then x else ma) xs

{-
accumArray ::  (e -> a -> e) -> e -> (Int, Int) -> [(Int, a)] -> Array e
accumArray f v (low,hi) = accum f (array (low,hi) (zip [low .. hi] (repeat v))) 

accum ::  (e -> a -> e) -> Array e -> [(Int, a)] -> Array e
accum external
-}



