module Parser where

infixl 2 `opt`
infixl 3 <|>, <||>
infixl 4 <$>, <$, <*>, *>, <*, <**>, <??>, <->>

type Parser s a = [s] -> [(a,[s])]

parse :: Parser s a -> [s] -> a
parse p s = fst . head $ filter (null . snd) (p s)


pSucceed :: a -> Parser s a
pSucceed v = (\ts -> [(v,ts)])

pFail :: Parser s a
pFail = (\_ -> [])

pPred :: (s -> Bool) -> Parser s s
pPred pred = p
 where
  p [] = []
  p (t:ts)
    | pred t    = [(t,ts)]
    | otherwise = []

pSym :: s -> Parser s s
pSym t = pPred (t==)

pAnyOf :: [s] -> Parser s s
pAnyOf = foldr (<|>) pFail . map pSym

opt :: Parser s a -> a -> Parser s a
p `opt` x = p <|> pSucceed x

(<|>) :: Parser s a -> Parser s a -> Parser s a
p <|> q = (\ts -> p ts ++ q ts)

(<||>) :: Parser s a -> Parser s a -> Parser s a
p <||> q = (\ts -> let res = p ts in if null res then q ts else res)

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
p <*> q = (\ts -> [ (f x,ts2) | (f,ts1) <- p ts, (x,ts2) <- q ts1 ])

(<$>) :: (a -> b) -> Parser s a -> Parser s b
f <$> p = pSucceed f <*> p

(<$) :: a -> Parser s b -> Parser s a
f <$ p = const f <$> p

(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = const id <$> p <*> q

(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = flip (const id) <$> p <*> q

(<**>) :: Parser s a -> Parser s (a -> b) -> Parser s b
p <**> q = flip ($) <$> p <*> q

(<??>) :: Parser s a -> Parser s (a->a) -> Parser s a
p <??> q = p <**> (q `opt` id)

pFoldr :: (a -> b -> b) -> b -> Parser s a -> Parser s b
pFoldr op e p = q where q = (op <$> p <*> q) `opt` e

pFoldrSep :: (a -> b -> b) -> b -> Parser s c -> Parser s a -> Parser s b
pFoldrSep op e sep p = (op <$> p <*> pFoldr op e (sep *> p)) `opt` e

pList :: Parser s a -> Parser s [a]
pList = pFoldr (:) []

pListSep :: Parser s a -> Parser s b -> Parser s [b]
pListSep = pFoldrSep (:) []

pSome :: Parser s a -> Parser s [a]
pSome p = (:) <$> p <*> pList p

(<->>) :: Parser s a -> (a -> Parser s b) -> Parser s b
p <->> f = (\ts -> [ res | (x,ts') <- p ts, res <- f x ts' ])

check :: (a -> Bool) -> Parser s a -> Parser s a
check pred (p) = (\ts -> filter (pred . fst) (p ts))

pAB = pSucceed 0 <|> pSym 'a' *> pAB <* pSym 'b'

main = parse pAB "aabb"


