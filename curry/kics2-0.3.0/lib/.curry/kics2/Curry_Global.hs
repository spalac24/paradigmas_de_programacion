{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Global (C_Global (..), C_GlobalSpec (..), d_C_readGlobal, d_C_writeGlobal, d_C_global) where

import Basics
import qualified Curry_Prelude
import Data.IORef
import System.IO
import System.Directory(doesFileExist)
import System.IO.Unsafe
import qualified Curry_Prelude as CP

-- Implementation of Globals in Curry. We use Haskell's IORefs for temporary
-- globals where Curry values are stored in the IORefs



data C_GlobalSpec
     = C_Temporary
     | C_Persistent (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_GlobalSpec Cover ID C_GlobalSpec C_GlobalSpec
     | Choices_C_GlobalSpec Cover ID ([C_GlobalSpec])
     | Fail_C_GlobalSpec Cover FailInfo
     | Guard_C_GlobalSpec Cover Constraints C_GlobalSpec

instance Show C_GlobalSpec where
  showsPrec d (Choice_C_GlobalSpec cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_GlobalSpec cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_GlobalSpec cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_GlobalSpec cd info) = showChar '!'
  showsPrec _ C_Temporary = showString "Temporary"
  showsPrec _ (C_Persistent x1) = (showString "(Persistent") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_GlobalSpec where
  readsPrec d s = (readParen False (\r -> [ (C_Temporary,r0) | (_,r0) <- readQualified "Global" "Temporary" r]) s) ++ (readParen (d > 10) (\r -> [ (C_Persistent x1,r1) | (_,r0) <- readQualified "Global" "Persistent" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_GlobalSpec where
  choiceCons = Choice_C_GlobalSpec
  choicesCons = Choices_C_GlobalSpec
  failCons = Fail_C_GlobalSpec
  guardCons = Guard_C_GlobalSpec
  try (Choice_C_GlobalSpec cd i x y) = tryChoice cd i x y
  try (Choices_C_GlobalSpec cd i xs) = tryChoices cd i xs
  try (Fail_C_GlobalSpec cd info) = Fail cd info
  try (Guard_C_GlobalSpec cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_GlobalSpec cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_GlobalSpec cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_GlobalSpec cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_GlobalSpec cd i _) = error ("Global.GlobalSpec.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_GlobalSpec cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_GlobalSpec cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_GlobalSpec where
  generate s c = Choices_C_GlobalSpec c (freeID [0,1] s) [C_Temporary,(C_Persistent (generate (leftSupply s) c))]


instance NormalForm C_GlobalSpec where
  ($!!) cont C_Temporary d cs = cont C_Temporary d cs
  ($!!) cont (C_Persistent x1) d cs = (((\y1 d cs -> cont (C_Persistent y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_GlobalSpec cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_GlobalSpec cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_GlobalSpec cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_GlobalSpec cd info) _ _ = failCons cd info
  ($##) cont C_Temporary d cs = cont C_Temporary d cs
  ($##) cont (C_Persistent x1) d cs = (((\y1 d cs -> cont (C_Persistent y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_GlobalSpec cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_GlobalSpec cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_GlobalSpec cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_GlobalSpec cd info) _ _ = failCons cd info
  searchNF _ cont C_Temporary = cont C_Temporary
  searchNF search cont (C_Persistent x1) = search (\y1 -> cont (C_Persistent y1)) x1
  searchNF _ _ x = error ("Global.GlobalSpec.searchNF: no constructor: " ++ (show x))


instance Unifiable C_GlobalSpec where
  (=.=) C_Temporary C_Temporary d cs = C_Success
  (=.=) (C_Persistent x1) (C_Persistent y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Temporary C_Temporary d cs = C_Success
  (=.<=) (C_Persistent x1) (C_Persistent y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Temporary = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (C_Persistent x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_GlobalSpec cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_GlobalSpec cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_GlobalSpec cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_GlobalSpec cd i _) = error ("Global.GlobalSpec.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_GlobalSpec cd info) = [(Unsolvable info)]
  bind d i (Guard_C_GlobalSpec cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Temporary = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (C_Persistent x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_GlobalSpec cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_GlobalSpec cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_GlobalSpec cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_GlobalSpec cd i _) = error ("Global.GlobalSpec.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_GlobalSpec cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_GlobalSpec cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_GlobalSpec where
  (=?=) (Choice_C_GlobalSpec cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_GlobalSpec cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_GlobalSpec cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_GlobalSpec cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_GlobalSpec cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_GlobalSpec cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_GlobalSpec cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_GlobalSpec cd info) _ _ = failCons cd info
  (=?=) C_Temporary C_Temporary d cs = Curry_Prelude.C_True
  (=?=) (C_Persistent x1) (C_Persistent y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_GlobalSpec cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_GlobalSpec cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_GlobalSpec cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_GlobalSpec cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_GlobalSpec cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_GlobalSpec cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_GlobalSpec cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_GlobalSpec cd info) _ _ = failCons cd info
  (<?=) C_Temporary C_Temporary d cs = Curry_Prelude.C_True
  (<?=) C_Temporary (C_Persistent _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Persistent x1) (C_Persistent y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_readGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_readGlobal x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_readGlobal x1 x3250 x3500

d_C_writeGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeGlobal x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_writeGlobal) x1 x3250 x3500) x2 x3250 x3500

d_C_global :: Curry_Prelude.Curry t0 => t0 -> C_GlobalSpec -> Cover -> ConstStore -> C_Global t0
d_C_global x1 x2 x3250 x3500 = external_d_C_global x1 x2 x3250 x3500

d_C_prim_readGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_prim_readGlobal x1 x3250 x3500 = external_d_C_prim_readGlobal x1 x3250 x3500

d_C_prim_writeGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_writeGlobal x1 x2 x3250 x3500 = external_d_C_prim_writeGlobal x1 x2 x3250 x3500
data C_Global a
     = Choice_C_Global Cover ID (C_Global a) (C_Global a)
     | Choices_C_Global Cover ID ([C_Global a])
     | Fail_C_Global Cover FailInfo
     | Guard_C_Global Cover Constraints (C_Global a)
     | C_Global_Temp (IORef a)  -- a temporary global
     | C_Global_Pers String     -- a persistent global with a given (file) name

instance Show (C_Global a) where
  show = error "ERROR: no show for Global"

instance Read (C_Global a) where
  readsPrec = error "ERROR: no read for Global"

instance NonDet (C_Global a) where
  choiceCons = Choice_C_Global
  choicesCons = Choices_C_Global
  failCons = Fail_C_Global
  guardCons = Guard_C_Global
  try (Choice_C_Global cd i x y) = tryChoice cd i x y
  try (Choices_C_Global cd i xs) = tryChoices cd i xs
  try (Fail_C_Global cd info) = Fail cd info
  try (Guard_C_Global cd c e) = Guard cd c e
  try x = Val x
  match choiceF _ _ _ _ _ (Choice_C_Global cd i x y) = choiceF cd i x y
  match _ narrF _ _ _ _   (Choices_C_Global cd i@(NarrowedID _ _) xs) 
   = narrF cd i xs
  match _ _ freeF _ _ _   (Choices_C_Global cd i@(FreeID _ _) xs)     
   = freeF cd i xs
  match _ _ _ failF _ _   (Fail_C_Global cd info) = failF cd info
  match _ _ _ _ guardF _  (Guard_C_Global cd c e) = guardF cd c e
  match _ _ _ _ _ valF    x                    = valF x

instance Generable (C_Global a) where
  generate _ _ = error "ERROR: no generator for Global"

instance NormalForm (C_Global a) where
  ($!!) cont g@(C_Global_Temp _)         cd cs = cont g cd cs
  ($!!) cont g@(C_Global_Pers _)         cd cs = cont g cd cs
  ($!!) cont (Choice_C_Global d i g1 g2) cd cs = nfChoice cont d i g1 g2 cd cs
  ($!!) cont (Choices_C_Global d i gs)   cd cs = nfChoices cont d i gs cd cs
  ($!!) cont (Guard_C_Global d c g)      cd cs = guardCons d c ((cont $!! g) cd 
                                                    $! (addCs c cs))
  ($!!) _    (Fail_C_Global d info)      _  _  = failCons d info
  ($##) cont g@(C_Global_Temp _)         cd cs = cont g cd cs
  ($##) cont g@(C_Global_Pers _)         cd cs = cont g cd cs
  ($##) cont (Choice_C_Global d i g1 g2) cd cs = gnfChoice cont d i g1 g2 cd cs
  ($##) cont (Choices_C_Global d i gs)   cd cs = gnfChoices cont d i gs cd cs
  ($##) cont (Guard_C_Global d c g)      cd cs = guardCons d c ((cont $## g) cd 
                                                    $!  (addCs c cs))
  ($##) _    (Fail_C_Global cd info)     _  _  = failCons cd info
  searchNF _ cont g@(C_Global_Temp _)          = cont g
  searchNF _ cont g@(C_Global_Pers _)          = cont g

instance Unifiable (C_Global a) where
  (=.=) (C_Global_Temp ref1) (C_Global_Temp ref2) _ _
    | ref1 == ref2 = C_Success
  (=.=) (C_Global_Pers f1) (C_Global_Pers f2) _ _
    | f1 == f2  = C_Success
  (=.=) _ _ cd _ = Fail_C_Success cd defFailInfo
  (=.<=) = (=.=)
  bind cd i (Choice_C_Global d j l r) 
    = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Global d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Global d j@(NarrowedID _ _) xs) 
    = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _ _ (Fail_C_Global _ info) = [Unsolvable info]
  bind cd i (Guard_C_Global _ cs e) = (getConstrList cs) ++ (bind cd i e)
  lazyBind cd i (Choice_C_Global d j l r) 
    = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Global d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Global d j@(NarrowedID _ _) xs) 
    = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _ _ (Fail_C_Global cd info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Global _ cs e) 
    = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance CP.Curry a => CP.Curry (C_Global a) where
  (=?=) = error "(==) is undefined for Globals"
  (<?=) = error "(<=) is undefined for Globals"


external_d_C_global :: CP.Curry a => a -> C_GlobalSpec -> Cover -> ConstStore 
                    -> C_Global a
external_d_C_global val C_Temporary _ _ = ref `seq` (C_Global_Temp ref)
  where ref = unsafePerformIO (newIORef val)
external_d_C_global val (C_Persistent cname) _ _ =
  let name = fromCurry cname :: String
   in unsafePerformIO (initGlobalFile name >> return (C_Global_Pers name))
 where initGlobalFile name = do
         ex <- doesFileExist name
         if ex then return ()
               else writeFile name (show val++"\n")

external_d_C_prim_readGlobal :: CP.Curry a => C_Global a -> Cover -> ConstStore 
                             -> CP.C_IO a
external_d_C_prim_readGlobal (C_Global_Temp  ref) _ _ = fromIO (readIORef ref)
external_d_C_prim_readGlobal (C_Global_Pers name) _ _ = fromIO $
  do h <- openFile name ReadMode
     s <- hGetLine h
     hClose h
     return (read s)

external_d_C_prim_writeGlobal :: CP.Curry a => C_Global a -> a
                              -> Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeGlobal (C_Global_Temp ref) val _ _ =
  toCurry (writeIORef ref val)
external_d_C_prim_writeGlobal (C_Global_Pers name) val _ _ =
  toCurry (writeFile name (show val ++ "\n"))

