import FlatCurry
import FlatCurryGoodies
import System
import ReadShowTerm
import Pretty
import PrettyStrict
import StyledText 
import List

main :: IO ()
main = getArgs >>= write . readTerm . head

write :: Int -> IO ()
write = writeFile (pc++".hs") . (header++) . mkPCs 

mkPCs :: Int -> String
mkPCs n = concatMap mkPC [1..n]

pc = "PartCalls"
dm = "DebuggerMonad"
pr = "Prelude"
te = "Term"

header = "module "++pc++" where\n\n\
         \import "++dm++"\n\
         \import "++te++"\n\n"

mkPC :: Int -> String
mkPC n = (plainText $ pretty 160 $ funcTypeDeclDoc pc (pc,"pc"++show n) 
                                 $ mkType n)++"\n"++
         (mkBody n)

infixr 7 ~>

(~~>), (~>) :: TypeExpr -> TypeExpr -> TypeExpr
x ~~> y = tc "Func" [x,y]
(~>) = FuncType

tc s = TCons (dm,s)
tc0 s = TCons (te,s) []

mkType :: Int -> TypeExpr
mkType n = tc0 "Term" ~> 
           foldr (\ v -> (TVar v ~>)) res [1..n] ~>
           foldr (\ v t -> TVar v ~~> t) (TVar (n+1)) [1..n]
  where res = tc "Debug" [TVar (n+1)]

ccc :: String -> String -> [Expr] -> Expr
ccc m s = Comb ConsCall (m,s)

cc,pcc :: String -> [Expr] -> Expr
cc  = ccc dm
pcc = ccc pr

app :: [Expr] -> Expr
app = Comb FuncCall (pr,"apply")

nil :: Expr
nil = pcc "[]" []

mkBody :: Int -> String
mkBody n = "pc"++show n++
               " (Term n xs) f =\n\
           \  Func (c []) "++concatMap showTerm [1..n-1]++
           end n

showTerm n = "(\\ x"++show n ++ " -> do\n\
             \  let sx"++show n++" = showTerm x"++show n++"\n\
             \  return (Func (c ["++sxs n++"]) "

sxs n = concat $ intersperse "," (map (\i->"sx"++show i) [1..n])

end n = "(\\ x"++show n++" ->\n\
        \    f"++concatMap (\i -> " x"++show i) [1..n]++
        replicate (2*n-1) ')'++"\n\
        \ where c = Term n . (xs++)\n\n"