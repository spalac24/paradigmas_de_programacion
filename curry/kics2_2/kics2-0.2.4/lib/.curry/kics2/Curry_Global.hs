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
  generate s = Choices_C_GlobalSpec defCover (freeID [0,1] s) [C_Temporary,(C_Persistent (generate (leftSupply s)))]


instance NormalForm C_GlobalSpec where
  ($!!) cont C_Temporary cs = cont C_Temporary cs
  ($!!) cont (C_Persistent x1) cs = ((\y1 cs -> cont (C_Persistent y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_GlobalSpec cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_GlobalSpec cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_GlobalSpec cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_GlobalSpec cd info) _ = failCons cd info
  ($##) cont C_Temporary cs = cont C_Temporary cs
  ($##) cont (C_Persistent x1) cs = ((\y1 cs -> cont (C_Persistent y1) cs) $## x1) cs
  ($##) cont (Choice_C_GlobalSpec cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_GlobalSpec cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_GlobalSpec cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_GlobalSpec cd info) _ = failCons cd info
  searchNF _ cont C_Temporary = cont C_Temporary
  searchNF search cont (C_Persistent x1) = search (\y1 -> cont (C_Persistent y1)) x1
  searchNF _ _ x = error ("Global.GlobalSpec.searchNF: no constructor: " ++ (show x))


instance Unifiable C_GlobalSpec where
  (=.=) C_Temporary C_Temporary cs = C_Success
  (=.=) (C_Persistent x1) (C_Persistent y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_Temporary C_Temporary cs = C_Success
  (=.<=) (C_Persistent x1) (C_Persistent y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_Temporary = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (C_Persistent x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_GlobalSpec cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_GlobalSpec cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_GlobalSpec cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_GlobalSpec cd i _) = error ("Global.GlobalSpec.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_GlobalSpec cd info) = [(Unsolvable info)]
  bind i (Guard_C_GlobalSpec cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_Temporary = [(i :=: (ChooseN 0 0))]
  lazyBind i (C_Persistent x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_GlobalSpec cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_GlobalSpec cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_GlobalSpec cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_GlobalSpec cd i _) = error ("Global.GlobalSpec.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_GlobalSpec cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_GlobalSpec cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_GlobalSpec where
  (=?=) (Choice_C_GlobalSpec cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_GlobalSpec cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_GlobalSpec cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_GlobalSpec cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_GlobalSpec cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_GlobalSpec cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_GlobalSpec cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_GlobalSpec cd info) _ = failCons cd info
  (=?=) C_Temporary C_Temporary cs = Curry_Prelude.C_True
  (=?=) (C_Persistent x1) (C_Persistent y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_GlobalSpec cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_GlobalSpec cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_GlobalSpec cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_GlobalSpec cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_GlobalSpec cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_GlobalSpec cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_GlobalSpec cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_GlobalSpec cd info) _ = failCons cd info
  (<?=) C_Temporary C_Temporary cs = Curry_Prelude.C_True
  (<?=) C_Temporary (C_Persistent _) _ = Curry_Prelude.C_True
  (<?=) (C_Persistent x1) (C_Persistent y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_GlobalSpec where
  cover C_Temporary = C_Temporary
  cover (C_Persistent x1) = C_Persistent (cover x1)
  cover (Choice_C_GlobalSpec cd i x y) = Choice_C_GlobalSpec (incCover cd) i (cover x) (cover y)
  cover (Choices_C_GlobalSpec cd i xs) = Choices_C_GlobalSpec (incCover cd) i (map cover xs)
  cover (Fail_C_GlobalSpec cd info) = Fail_C_GlobalSpec (incCover cd) info
  cover (Guard_C_GlobalSpec cd c e) = Guard_C_GlobalSpec (incCover cd) c (cover e)


d_C_readGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> ConstStore -> Curry_Prelude.C_IO t0
d_C_readGlobal x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_readGlobal x1 x3500

d_C_writeGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> t0 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeGlobal x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_writeGlobal) x1 x3500) x2 x3500

d_C_global :: Curry_Prelude.Curry t0 => t0 -> C_GlobalSpec -> ConstStore -> C_Global t0
d_C_global x1 x2 x3500 = external_d_C_global x1 x2 x3500

d_C_prim_readGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> ConstStore -> Curry_Prelude.C_IO t0
d_C_prim_readGlobal x1 x3500 = external_d_C_prim_readGlobal x1 x3500

d_C_prim_writeGlobal :: Curry_Prelude.Curry t0 => C_Global t0 -> t0 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_writeGlobal x1 x2 x3500 = external_d_C_prim_writeGlobal x1 x2 x3500
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
  match _ narrF _ _ _ _   (Choices_C_Global cd i@(NarrowedID _ _) xs) = narrF cd i xs
  match _ _ freeF _ _ _   (Choices_C_Global cd i@(FreeID _ _) xs)     = freeF cd i xs
  match _ _ _ failF _ _   (Fail_C_Global cd info) = failF cd info
  match _ _ _ _ guardF _  (Guard_C_Global cd c e) = guardF cd c e
  match _ _ _ _ _ valF    x                    = valF x

instance Generable (C_Global a) where
  generate _ = error "ERROR: no generator for Global"

instance NormalForm (C_Global a) where
  ($!!) cont g@(C_Global_Temp _)          cs = cont g cs
  ($!!) cont g@(C_Global_Pers _)          cs = cont g cs
  ($!!) cont (Choice_C_Global cd i g1 g2) cs = nfChoice cont cd i g1 g2 cs
  ($!!) cont (Choices_C_Global cd i gs)   cs = nfChoices cont cd i gs cs
  ($!!) cont (Guard_C_Global cd c g)      cs = guardCons cd c ((cont $!! g) (addCs c cs))
  ($!!) _    (Fail_C_Global cd info)      cs = failCons cd info
  ($##) cont g@(C_Global_Temp _)          cs = cont g cs
  ($##) cont g@(C_Global_Pers _)          cs = cont g cs
  ($##) cont (Choice_C_Global cd i g1 g2) cs = gnfChoice cont cd i g1 g2 cs
  ($##) cont (Choices_C_Global cd i gs)   cs = gnfChoices cont cd i gs cs
  ($##) cont (Guard_C_Global cd c g)      cs = guardCons cd c ((cont $## g) (addCs c cs))
  ($##) _    (Fail_C_Global cd info)      cs = failCons cd info
  searchNF _ cont g@(C_Global_Temp _)        = cont g
  searchNF _ cont g@(C_Global_Pers _)        = cont g

instance Unifiable (C_Global a) where
  (=.=) (C_Global_Temp ref1) (C_Global_Temp ref2) _
    | ref1 == ref2 = C_Success
  (=.=) (C_Global_Pers f1) (C_Global_Pers f2) _
    | f1 == f2  = C_Success
  (=.=) _ _ _ = Fail_C_Success 0 defFailInfo
  (=.<=) = (=.=)
  bind i (Choice_C_Global cd j l r) = [(ConstraintChoice cd j (bind i l) (bind i r))]
  bind i (Choices_C_Global cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Global cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Fail_C_Global cd info) = [Unsolvable info]
  bind i (Guard_C_Global _ cs e) = (getConstrList cs) ++ (bind i e)
  lazyBind i (Choice_C_Global cd j l r) = [(ConstraintChoice cd j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Global cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Global cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Fail_C_Global cd info) = [Unsolvable info]
  lazyBind i (Guard_C_Global _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind i e)))]

instance CP.Curry a => CP.Curry (C_Global a) where
  (=?=) = error "(==) is undefined for Globals"
  (<?=) = error "(<=) is undefined for Globals"

instance Coverable (C_Global a) where
  cover (Choice_C_Global cd i x y) = Choice_C_Global (incCover cd) i (cover x) (cover y) 
  cover (Choices_C_Global cd i xs) = Choices_C_Global (incCover cd) i (map cover xs)
  cover (Fail_C_Global cd info)    = Fail_C_Global (incCover cd) info 
  cover (Guard_C_Global cd cs x)   = Guard_C_Global (incCover cd) cs (cover x)     
  cover x@(C_Global_Temp _)        = x
  cover x@(C_Global_Pers _)        = x


external_d_C_global :: CP.Curry a => a -> C_GlobalSpec -> ConstStore -> C_Global a
external_d_C_global val C_Temporary _ = ref `seq` (C_Global_Temp ref)
  where ref = unsafePerformIO (newIORef val)
external_d_C_global val (C_Persistent cname) _ =
  let name = fromCurry cname :: String
   in unsafePerformIO (initGlobalFile name >> return (C_Global_Pers name))
 where initGlobalFile name = do
         ex <- doesFileExist name
         if ex then return ()
               else writeFile name (show val++"\n")

external_d_C_prim_readGlobal :: CP.Curry a => C_Global a -> ConstStore -> CP.C_IO a
external_d_C_prim_readGlobal (C_Global_Temp  ref) _ = fromIO (readIORef ref)
external_d_C_prim_readGlobal (C_Global_Pers name) _ = fromIO $
  do h <- openFile name ReadMode
     s <- hGetLine h
     hClose h
     return (read s)

external_d_C_prim_writeGlobal :: CP.Curry a => C_Global a -> a
                                            -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeGlobal (C_Global_Temp ref) val _ =
  toCurry (writeIORef ref val)
external_d_C_prim_writeGlobal (C_Global_Pers name) val _ =
  toCurry (writeFile name (show val ++ "\n"))

