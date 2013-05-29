{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CategorizedHtmlList (d_C_list2CategorizedHtml, nd_C_list2CategorizedHtml, d_C_categorizeByItemKey, nd_C_categorizeByItemKey, d_C_stringList2ItemList, nd_C_stringList2ItemList) where

import Basics
import qualified Curry_Char
import qualified Curry_HTML
import qualified Curry_List
import qualified Curry_Prelude
d_C_list2CategorizedHtml :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> (t0 -> ConstStore -> t1 -> ConstStore -> Curry_Prelude.C_Bool) -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_C_list2CategorizedHtml x1 x2 x3 x3500 = Curry_Prelude.OP_Cons (d_C_categories2LinkList x2 x3500) (Curry_Prelude.d_C_map (d_OP_list2CategorizedHtml_dot___hash_lambda1 x3 x2 x1) x2 x3500)

nd_C_list2CategorizedHtml :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Func t0 (Func t1 Curry_Prelude.C_Bool) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_C_list2CategorizedHtml x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_categories2LinkList x2 x2000 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_list2CategorizedHtml_dot___hash_lambda1 x3 x2 x1)) x2 x2001 x3500))))))

d_OP_list2CategorizedHtml_dot___hash_lambda1 :: (Curry_Prelude.Curry t34,Curry_Prelude.Curry t18) => (t34 -> ConstStore -> t18 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t18 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t34 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)) -> Curry_Prelude.OP_Tuple2 t18 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_HTML.C_HtmlExp
d_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_HTML.d_C_anchor (Curry_HTML.d_C_string2urlencoded (Curry_Prelude.d_C_show x5 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_HTML.d_C_h2 (Curry_Prelude.OP_Cons (Curry_HTML.d_C_htxt x6 x3500) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x3500) (Curry_Prelude.d_C_filter (d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x5) x3 x3500) x3500) (Curry_Prelude.OP_Cons (d_C_categories2LinkList x2 x3500) Curry_Prelude.OP_List) x3500)) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x1002 x3500) (d_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_list2CategorizedHtml_dot___hash_lambda1 :: (Curry_Prelude.Curry t34,Curry_Prelude.Curry t18) => Func t34 (Func t18 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t18 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t34 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)) -> Curry_Prelude.OP_Tuple2 t18 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_HTML.C_HtmlExp
nd_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2012 = x3000
           in (seq x2012 (let
               x2011 = leftSupply x2012
               x2010 = rightSupply x2012
                in (seq x2011 (seq x2010 (Curry_HTML.nd_C_anchor (Curry_HTML.d_C_string2urlencoded (Curry_Prelude.d_C_show x5 x3500) x3500) (let
                    x2002 = leftSupply x2010
                    x2009 = rightSupply x2010
                     in (seq x2002 (seq x2009 (Curry_Prelude.OP_Cons (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_HTML.nd_C_h2 (Curry_Prelude.OP_Cons (Curry_HTML.nd_C_htxt x6 x2000 x3500) Curry_Prelude.OP_List) x2001 x3500)))) (let
                         x2006 = leftSupply x2009
                         x2008 = rightSupply x2009
                          in (seq x2006 (seq x2008 (Curry_Prelude.d_OP_plus_plus (let
                              x2005 = leftSupply x2006
                              x2007 = rightSupply x2006
                               in (seq x2005 (seq x2007 (let
                                   x2003 = leftSupply x2007
                                   x2004 = rightSupply x2007
                                    in (seq x2003 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2) x2003 x3500) (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x5)) x3 x2004 x3500) x2005 x3500))))))) (Curry_Prelude.OP_Cons (nd_C_categories2LinkList x2 x2008 x3500) Curry_Prelude.OP_List) x3500)))))))) x2011 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x1002 x3000 x3500) (nd_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_list2CategorizedHtml_dot___hash_lambda1 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.Curry t34 => Curry_Prelude.OP_Tuple2 t34 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_HTML.d_C_breakline x3500) Curry_Prelude.OP_List) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1002 x3500) (d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.Curry t34 => Curry_Prelude.OP_Tuple2 t34 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_HTML.nd_C_breakline x2000 x3500) Curry_Prelude.OP_List) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1002 x3000 x3500) (nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 :: (Curry_Prelude.Curry t18,Curry_Prelude.Curry t34) => (t34 -> ConstStore -> t18 -> ConstStore -> Curry_Prelude.C_Bool) -> t18 -> Curry_Prelude.OP_Tuple2 t34 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> ConstStore -> Curry_Prelude.C_Bool
d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3500) x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x1002 x3500) (d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 :: (Curry_Prelude.Curry t18,Curry_Prelude.Curry t34) => Func t34 (Func t18 Curry_Prelude.C_Bool) -> t18 -> Curry_Prelude.OP_Tuple2 t34 (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) x2 x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x1002 x3000 x3500) (nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_list2CategorizedHtml_dot___hash_lambda1_dot___hash_lambda3 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_categories2LinkList :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_HTML.C_HtmlExp
d_C_categories2LinkList x1 x3500 = Curry_HTML.d_C_par (Curry_Prelude.OP_Cons (Curry_HTML.d_C_center (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_categories2LinkList_dot___hash_lambda4 x3500) x1 x3500) x3500) Curry_Prelude.OP_List) x3500

nd_C_categories2LinkList :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> Curry_HTML.C_HtmlExp
nd_C_categories2LinkList x1 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_HTML.nd_C_par (Curry_Prelude.OP_Cons (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_HTML.nd_C_center (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id nd_OP_categories2LinkList_dot___hash_lambda4) x2000 x3500) x1 x2001 x3500)))) x2003 x3500)))) Curry_Prelude.OP_List) x2005 x3500)))))

d_OP_categories2LinkList_dot___hash_lambda4 :: Curry_Prelude.Curry t6 => Curry_Prelude.OP_Tuple2 t6 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_OP_categories2LinkList_dot___hash_lambda4 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.OP_Cons (Curry_HTML.d_C_href (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_HTML.d_C_string2urlencoded (Curry_Prelude.d_C_show x2 x3500) x3500)) (Curry_Prelude.OP_Cons (Curry_HTML.d_C_htxt x3 x3500) Curry_Prelude.OP_List) x3500) (Curry_Prelude.OP_Cons (Curry_HTML.d_C_nbsp x3500) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_categories2LinkList_dot___hash_lambda4 x1002 x3500) (d_OP_categories2LinkList_dot___hash_lambda4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_categories2LinkList_dot___hash_lambda4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_categories2LinkList_dot___hash_lambda4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_categories2LinkList_dot___hash_lambda4 :: Curry_Prelude.Curry t6 => Curry_Prelude.OP_Tuple2 t6 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_OP_categories2LinkList_dot___hash_lambda4 x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (Curry_Prelude.OP_Cons (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_HTML.nd_C_href (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_HTML.d_C_string2urlencoded (Curry_Prelude.d_C_show x2 x3500) x3500)) (Curry_Prelude.OP_Cons (Curry_HTML.nd_C_htxt x3 x2000 x3500) Curry_Prelude.OP_List) x2001 x3500)))) (Curry_Prelude.OP_Cons (Curry_HTML.nd_C_nbsp x2003 x3500) Curry_Prelude.OP_List))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_categories2LinkList_dot___hash_lambda4 x1002 x3000 x3500) (nd_OP_categories2LinkList_dot___hash_lambda4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_categories2LinkList_dot___hash_lambda4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_categories2LinkList_dot___hash_lambda4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_categorizeByItemKey :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)) -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
d_C_categorizeByItemKey x1 x3500 = d_C_list2CategorizedHtml x1 (Curry_Prelude.d_C_map d_OP_categorizeByItemKey_dot___hash_lambda5 (Curry_Prelude.d_C_apply (d_C_listHeads x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x1 x3500) x3500) x3500) (acceptCs id d_C_categorizeStringHead) x3500

nd_C_categorizeByItemKey :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_HTML.C_HtmlExp
nd_C_categorizeByItemKey x1 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2007 = leftSupply x2008
          x2006 = rightSupply x2008
           in (seq x2007 (seq x2006 (nd_C_list2CategorizedHtml x1 (let
               x2005 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2005 (seq x2003 (Curry_Prelude.nd_C_map (wrapDX id d_OP_categorizeByItemKey_dot___hash_lambda5) (let
                    x2002 = leftSupply x2003
                    x2004 = rightSupply x2003
                     in (seq x2002 (seq x2004 (let
                         x2000 = leftSupply x2004
                         x2001 = rightSupply x2004
                          in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_listHeads x2000 x3500) (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst) x1 x2001 x3500) x2002 x3500))))))) x2005 x3500)))) (wrapDX (wrapDX id) (acceptCs id d_C_categorizeStringHead)) x2007 x3500)))))

d_OP_categorizeByItemKey_dot___hash_lambda5 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Char (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_categorizeByItemKey_dot___hash_lambda5 x1 x3500 = Curry_Prelude.OP_Tuple2 (Curry_Char.d_C_toUpper x1 x3500) (Curry_Prelude.OP_Cons (Curry_Char.d_C_toUpper x1 x3500) Curry_Prelude.OP_List)

d_C_stringList2ItemList :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp))
d_C_stringList2ItemList x3500 = Curry_Prelude.d_C_map d_OP_stringList2ItemList_dot___hash_lambda6

nd_C_stringList2ItemList :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)))
nd_C_stringList2ItemList x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_map (wrapNX id nd_OP_stringList2ItemList_dot___hash_lambda6))

d_OP_stringList2ItemList_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
d_OP_stringList2ItemList_dot___hash_lambda6 x1 x3500 = Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.OP_Cons (Curry_HTML.d_C_htxt x1 x3500) Curry_Prelude.OP_List)

nd_OP_stringList2ItemList_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_HTML.C_HtmlExp)
nd_OP_stringList2ItemList_dot___hash_lambda6 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.OP_Cons (Curry_HTML.nd_C_htxt x1 x2000 x3500) Curry_Prelude.OP_List)))

d_C_listHeads :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_listHeads x3500 = Curry_Prelude.d_OP_dot (Curry_List.d_C_nubBy (acceptCs id d_C_isUpperEqual)) (Curry_Prelude.d_C_foldr (acceptCs id d_OP_listHeads_dot___hash_lambda7) Curry_Prelude.OP_List) x3500

nd_C_listHeads :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_listHeads x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_List.nd_C_nubBy (wrapDX (wrapDX id) (acceptCs id d_C_isUpperEqual)))) (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_listHeads_dot___hash_lambda7)) Curry_Prelude.OP_List)) x2000 x3500))

d_OP_listHeads_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_listHeads_dot___hash_lambda7 x1 x2 x3500 = d_OP__case_0 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 Curry_Prelude.OP_List x3500) x3500

d_C_categorizeStringHead :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_categorizeStringHead x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x3 x4) -> d_C_isUpperEqual x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_categorizeStringHead x1002 x2 x3500) (d_C_categorizeStringHead x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_categorizeStringHead z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_categorizeStringHead x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isUpperEqual :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_isUpperEqual x1 x2 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Char.d_C_toUpper x1 x3500) (Curry_Char.d_C_toUpper x2 x3500) x3500

d_C_main :: ConstStore -> Curry_Prelude.C_IO Curry_HTML.C_HtmlForm
d_C_main x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_HTML.d_C_form (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))) (d_C_categorizeByItemKey (Curry_Prelude.d_C_apply (d_C_stringList2ItemList x3500) (d_C_testList x3500) x3500) x3500) x3500) x3500

nd_C_main :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_HTML.C_HtmlForm
nd_C_main x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2007 = leftSupply x2008
          x2006 = rightSupply x2008
           in (seq x2007 (seq x2006 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_return) (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_HTML.nd_C_form (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))) (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (nd_C_categorizeByItemKey (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_stringList2ItemList x2000 x3500) (d_C_testList x3500) x2001 x3500)))) x2003 x3500)))) x2005 x3500)))) x2007 x3500)))))

d_C_testList :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_testList x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'Q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List))))))))))))

d_OP__case_0 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_head x1 x3500) x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x1002 x3500) (d_OP__case_0 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_head x1 x3500) x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
