module Term where 

import Maybe
import Text.PrettyPrint 
import Store
import Debug.Trace
import AnsiCodes
import Char

trace' x = trace (show x) x

type FontSetting = String

data Term
  = Term String [Term]
  | Var VarIndex Term
  | Underscore 
  | FailTerm String
  | OrTerm OrRef [Term] deriving Eq


par True  x = parens x
par False x = x

prettyTerm :: FontSetting -> Int -> Term -> Doc
prettyTerm _ _ Underscore = text "_"
prettyTerm fs _ (Term "_" []) = text green <> text "_" <> text fs
prettyTerm _ _ (Term "Zero" []) = text "0"
prettyTerm _ _ (Term "Pos" [n]) 
  | isCompleteNatnum n = text (show (parseNat n))
prettyTerm _ _ (Term "Neg" [n]) 
  | isCompleteNatnum n = text ('-':show (parseNat n))
prettyTerm _ _ (Term "IHi" []) = text "1"
prettyTerm _ _ n@(Term "I" [_]) 
  | isCompleteNatnum n = text (show (parseNat n))
prettyTerm _ _ n@(Term "O" [_]) 
  | isCompleteNatnum n = text (show (parseNat n))
prettyTerm _ _ (Term s [])      = text s
prettyTerm fs _ t@(Term ":" [Term ('\'':x) [],_]) = text "\"" <> prettyString fs t
prettyTerm fs _ t@(Term ":" [_,xs])  
  | isCompleteList xs = text "[" <> fcat (prettyList fs t)
prettyTerm fs _ (Term ('(':cs) xs) 
  | dropWhile (==',') cs == ")" = 
  parens (fcat (punctuate comma (map (prettyTerm fs 0) xs)))
prettyTerm fs _ (Term name [x])
 | isInfixOpName name = 
 parens (prettyTerm fs 6 x <> text name)
prettyTerm fs d (Term name [x,y]) 
 | isInfixOpName name = 
 par (d>5) 
     (sep [prettyTerm fs 6 x,text name,prettyTerm fs 6 y])
prettyTerm fs d (Term name (x:y:xs)) 
 | isInfixOpName name = 
 par (d>5) 
     (sep (parens (sep [prettyTerm fs 6 x,text name,prettyTerm fs 6 y]):
             map (prettyTerm fs 6) xs))
prettyTerm fs d (Term name args) = 
 par (d>10) (hang (text name) 2 (sep (map (prettyTerm fs 11) args)))
--prettyTerm fs d (OrTerm r (x:xs)) | isGenerator r = 
-- text [chr (deref r+65)]
 --par (d>5) (sep (prettyTerm fs 6 x:text "?":map (prettyTerm fs 6) xs))
prettyTerm fs d (OrTerm r (x:xs)) = 
 par (d>5) (sep (prettyTerm fs 6 x:text "?":map (prettyTerm fs 6) xs))
prettyTerm fs d (FailTerm s) = 
 par (d>10) (text (magenta++"error") <+> doubleQuotes (text s) <> text fs)


prettyList :: FontSetting -> Term -> [Doc]
prettyList fs (Term ":" [x,Term "[]" []]) = [prettyTerm fs 0 x <> text "]"]
prettyList fs (Term ":" [x,y]) = prettyTerm fs 0 x <> comma : prettyList fs y
prettyList fs t = [prettyTerm fs 0 t]

prettyString :: FontSetting -> Term -> Doc
prettyString _ (Term "[]" []) = text "\""
prettyString fs (Term ":" [x,y]) = prettyChar fs x <> prettyString fs y
prettyString fs t = prettyTerm fs 0 t

prettyChar _ (Term x []) = case reads x::[(Char,String)] of
  [('"',"")] -> text "\\\""  --"
  [(c,"")]   
       | oc <= 7   -> text ("\\00"++show oc)
       | oc <= 10  -> text (showLitChar c "")
       | oc <= 12  -> text ("\\0"++show oc)
       | oc <= 13  -> text (showLitChar c "")
       | oc <= 31  -> text ("\\0"++show oc)
       | oc <= 126 -> text (showLitChar c "")
       | otherwise -> text ("\\"++show oc)
       where oc = ord c
  _          -> empty
prettyChar fs t = prettyTerm fs 0 t

instance Show Term where
 showsPrec d t c = fullRender OneLineMode undefined undefined string_txt c (prettyTerm "" d t)
  where
    string_txt (Chr c)   s  = c:s
    string_txt (Str s1)  s2 = s1 ++ s2
    string_txt (PStr s1) s2 = s1 ++ s2


restrict :: Term -> Int -> Term
restrict t@(Term s []) _     = t 
restrict (Term _ _)    0     = Term "..." []
restrict (Term s ts)   (n+1) = Term s (map (flip restrict n) ts)
restrict t             _     = t

type VarIndex = Either Int String

parseNat :: Term -> Integer
parseNat (Term "I" [c]) = 2 * parseNat c + 1
parseNat (Term "O" [c]) = 2 * parseNat c
parseNat (Term "IHi" _) = 1
            
isCompleteNatnum :: Term -> Bool
isCompleteNatnum (Term c args) = 
  elem c ["I","O","IHi"] && all isCompleteNatnum args
isCompleteNatnum _ = False

isCompleteList :: Term -> Bool
isCompleteList (Var _ t)    = False 
isCompleteList (FailTerm _) = False
isCompleteList Underscore   = False
isCompleteList (OrTerm _ _)     = False
isCompleteList (Term str xs) 
   | str == "[]" = True
   | length xs < 2 = False
   | str == ":" = isCompleteList (head (tail xs))
   | otherwise = True
   
areAllElementsChars :: Term -> Bool
areAllElementsChars (Term _ []) = True
areAllElementsChars (Term _ [elem,list]) = 
  case elem of 
    Term ('\'':s) [] -> areAllElementsChars list
    _                -> False

isString :: Term -> Bool
isString (Term _ []) = False
isString t@(Term _ [_,_]) = areAllElementsChars t

isInfixOpName :: String -> Bool
isInfixOpName = all (`elem` infixIDs)

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"
        
trusted :: Term
trusted = Term "_" []

------------------------------------------------------
-- generic traversals
------------------------------------------------------

trTerm :: a -> (String -> a) -> (OrRef -> [a] -> a) 
       -> (String -> [a] -> a) -> Term -> a
trTerm und _ _ _ Underscore = und 
trTerm _ failf _ _ (FailTerm s) = failf s
trTerm und failf orf termf (OrTerm r ds) = 
  orf r (map (trTerm und failf orf termf) ds)
trTerm und failf orf termf (Term s xs) =  
  termf s (map (trTerm und failf orf termf) xs)


------------------------------------------------------
-- bubbling up shared or nodes
------------------------------------------------------


--bubble :: Term -> Term
bubble term = if changed then bubble newTerm else term
  where
    (changed,(_,newTerm)) = 
      trTerm (False,([],Underscore)) 
             (\ s -> (False,([],FailTerm s))) 
             ors datas term

    ors r bsrsds = 
      let (bubbs,rsds) = unzip bsrsds
       in (or bubbs,([r],OrTerm r (map snd rsds)))
    datas nr bsrsds = 
      let (bubbs,rsds) = unzip bsrsds
          rss = map fst rsds
          (twices,rs) = (dups (merge rss))
       in (or (not (null twices):bubbs),
           (rs,bubbleUp nr twices rsds))

    bubbleUp nr [] rsds  =  Term nr (map snd rsds)
    bubbleUp nr (i:is) rsds = 
       let newRsDs  = transpose (map (draw i) rsds)
           branches = map (bubbleUp nr is) newRsDs
        in OrTerm i branches

    draw i empty@([],d) = repeat empty
    draw i rrsd@(r:rs,d) = case compare i r of
          LT -> repeat rrsd
          EQ -> map ((,) rs) (drawUp i d)
          GT -> draw i (rs,d)


-- different for infinite lists from List.transpose
transpose :: [[a]] -> [[a]]
transpose xss | any null xss = []
              | otherwise = map head xss : transpose (map tail xss)

merge :: Ord a => [[a]] -> [a]
merge []   = []
merge [xs] = xs
merge xsys = merge (mergePairs xsys)
  where
    mergePairs []   = []
    mergePairs [xs] = [xs]
    mergePairs (xs:ys:xsys) = mergeTwo xs ys : mergePairs xsys

    mergeTwo [] ys = ys
    mergeTwo xs [] = xs
    mergeTwo xs@(x:xs') ys@(y:ys') = 
      if x<=y then x : mergeTwo xs' ys
              else y : mergeTwo xs  ys'


dups :: Ord a => [a] -> ([a],[a])
dups = snd 
     . foldr (\ x (p,(ds,us)) -> 
              ((x==),
               if p x then (x:dropWhile (x==) ds,us) else (ds,x:us)))
             (\_->False,([],[]))
                           
     
drawUp :: OrRef -> Term -> [Term]
drawUp i = either (error "nothing to draw") id . 
           trTerm (Left Underscore) (Left . FailTerm) ors datas
  where
    ors :: OrRef -> [Either Term [Term]] -> Either Term [Term]
    ors j ts | i==j      = Right (map (either id errmsg) ts)
             | otherwise = eit (OrTerm j) (eithers ts)

    datas nr ds = eit (Term nr) $ eithers ds

    errmsg = error "circle in or references"

eithers :: [Either a [a]] -> Either [a] [[a]]
eithers [] = Left []
eithers (Left x:ys)   = eit (x:) (eithers ys)
eithers (Right xs:ys) = 
  either (\ ys  -> Right (map (:ys) xs))
         (\ yss -> Right [ x:ys | x <- xs, ys <- yss]) (eithers ys)

eit :: (a -> b) -> Either a [a] -> Either b [b]
eit f = either (Left . f) (Right . map f)



 