{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_ERDGoodies (d_C_erdName, d_C_entityName, d_C_isEntityNamed, d_C_hasForeignKey, d_C_foreignKeyAttributes, d_C_entityAttributes, d_C_attributeName, d_C_attributeDomain, d_C_hasDefault, d_C_isForeignKey, d_C_isNullAttribute, d_C_cardMinimum, d_C_cardMaximum, d_C_showERD, d_C_combineIds) where

import Basics
import qualified Curry_Char
import qualified Curry_ERD
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
d_C_erdName :: Curry_ERD.C_ERD -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_erdName x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_ERD x2 x3 x4) -> x2
     (Curry_ERD.Choice_C_ERD x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_erdName x1002 x3250 x3500) (d_C_erdName x1003 x3250 x3500)
     (Curry_ERD.Choices_C_ERD x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_erdName z x3250 x3500) x1002
     (Curry_ERD.Guard_C_ERD x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_erdName x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_ERD x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_entityName :: Curry_ERD.C_Entity -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_entityName x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Entity x2 x3) -> x2
     (Curry_ERD.Choice_C_Entity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_entityName x1002 x3250 x3500) (d_C_entityName x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Entity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_entityName z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Entity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_entityName x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Entity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isEntityNamed :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Entity -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEntityNamed x1 x2 x3250 x3500 = Curry_Prelude.d_OP_eq_eq (d_C_entityName x2 x3250 x3500) x1 x3250 x3500

d_C_hasForeignKey :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Entity -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_hasForeignKey x1 x2 x3250 x3500 = case x2 of
     (Curry_ERD.C_Entity x3 x4) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_OP_hasForeignKey_dot_isForeignKeyWithName_dot_12 x1) x3250 x3500) x4 x3250 x3500
     (Curry_ERD.Choice_C_Entity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hasForeignKey x1 x1002 x3250 x3500) (d_C_hasForeignKey x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Entity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hasForeignKey x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Entity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hasForeignKey x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Entity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_hasForeignKey_dot_isForeignKeyWithName_dot_12 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_hasForeignKey_dot_isForeignKeyWithName_dot_12 x1 x2 x3250 x3500 = case x2 of
     (Curry_ERD.C_Attribute x3 x4 x5 x6) -> d_OP__case_6 x1 x4 x3250 x3500
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_hasForeignKey_dot_isForeignKeyWithName_dot_12 x1 x1002 x3250 x3500) (d_OP_hasForeignKey_dot_isForeignKeyWithName_dot_12 x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_hasForeignKey_dot_isForeignKeyWithName_dot_12 x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_hasForeignKey_dot_isForeignKeyWithName_dot_12 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_foreignKeyAttributes :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_ERD.C_Attribute
d_C_foreignKeyAttributes x1 x2 x3250 x3500 = Curry_Prelude.d_C_filter (d_OP_foreignKeyAttributes_dot_isForeignKeyWithName_dot_24 x1) x2 x3250 x3500

d_OP_foreignKeyAttributes_dot_isForeignKeyWithName_dot_24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_foreignKeyAttributes_dot_isForeignKeyWithName_dot_24 x1 x2 x3250 x3500 = case x2 of
     (Curry_ERD.C_Attribute x3 x4 x5 x6) -> d_OP__case_5 x1 x4 x3250 x3500
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_foreignKeyAttributes_dot_isForeignKeyWithName_dot_24 x1 x1002 x3250 x3500) (d_OP_foreignKeyAttributes_dot_isForeignKeyWithName_dot_24 x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_foreignKeyAttributes_dot_isForeignKeyWithName_dot_24 x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_foreignKeyAttributes_dot_isForeignKeyWithName_dot_24 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_foreignKeyAttrNames :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_foreignKeyAttrNames x1 x2 x3250 x3500 = Curry_Prelude.d_C_map d_C_attributeName (Curry_Prelude.d_C_filter (d_OP_foreignKeyAttrNames_dot_isForeignKeyWithName_dot_36 x1) x2 x3250 x3500) x3250 x3500

d_OP_foreignKeyAttrNames_dot_isForeignKeyWithName_dot_36 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_foreignKeyAttrNames_dot_isForeignKeyWithName_dot_36 x1 x2 x3250 x3500 = case x2 of
     (Curry_ERD.C_Attribute x3 x4 x5 x6) -> d_OP__case_4 x1 x4 x3250 x3500
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_foreignKeyAttrNames_dot_isForeignKeyWithName_dot_36 x1 x1002 x3250 x3500) (d_OP_foreignKeyAttrNames_dot_isForeignKeyWithName_dot_36 x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_foreignKeyAttrNames_dot_isForeignKeyWithName_dot_36 x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_foreignKeyAttrNames_dot_isForeignKeyWithName_dot_36 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_entityAttributes :: Curry_ERD.C_Entity -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_ERD.C_Attribute
d_C_entityAttributes x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Entity x2 x3) -> x3
     (Curry_ERD.Choice_C_Entity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_entityAttributes x1002 x3250 x3500) (d_C_entityAttributes x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Entity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_entityAttributes z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Entity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_entityAttributes x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Entity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_attributeName :: Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_attributeName x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Attribute x2 x3 x4 x5) -> x2
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_attributeName x1002 x3250 x3500) (d_C_attributeName x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_attributeName z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_attributeName x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_attributeDomain :: Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_ERD.C_Domain
d_C_attributeDomain x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Attribute x2 x3 x4 x5) -> x3
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_attributeDomain x1002 x3250 x3500) (d_C_attributeDomain x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_attributeDomain z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_attributeDomain x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_hasDefault :: Curry_ERD.C_Domain -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_hasDefault x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_IntDom x2) -> Curry_Maybe.d_C_isJust x2 x3250 x3500
     (Curry_ERD.C_FloatDom x3) -> Curry_Maybe.d_C_isJust x3 x3250 x3500
     (Curry_ERD.C_StringDom x4) -> Curry_Maybe.d_C_isJust x4 x3250 x3500
     (Curry_ERD.C_BoolDom x5) -> Curry_Maybe.d_C_isJust x5 x3250 x3500
     (Curry_ERD.C_DateDom x6) -> Curry_Maybe.d_C_isJust x6 x3250 x3500
     (Curry_ERD.C_UserDefined x7 x8) -> Curry_Maybe.d_C_isJust x8 x3250 x3500
     (Curry_ERD.Choice_C_Domain x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hasDefault x1002 x3250 x3500) (d_C_hasDefault x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Domain x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hasDefault z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Domain x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hasDefault x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Domain x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isForeignKey :: Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isForeignKey x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Attribute x2 x3 x4 x5) -> d_OP__case_3 x3 x3250 x3500
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isForeignKey x1002 x3250 x3500) (d_C_isForeignKey x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isForeignKey z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isForeignKey x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isNullAttribute :: Curry_ERD.C_Attribute -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isNullAttribute x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Attribute x2 x3 x4 x5) -> x5
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isNullAttribute x1002 x3250 x3500) (d_C_isNullAttribute x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isNullAttribute z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isNullAttribute x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_cardMinimum :: Curry_ERD.C_Cardinality -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_cardMinimum x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Exactly x2) -> x2
     (Curry_ERD.C_Between x3 x4) -> x3
     (Curry_ERD.Choice_C_Cardinality x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cardMinimum x1002 x3250 x3500) (d_C_cardMinimum x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Cardinality x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cardMinimum z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Cardinality x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cardMinimum x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Cardinality x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_cardMaximum :: Curry_ERD.C_Cardinality -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_cardMaximum x1 x3250 x3500 = case x1 of
     (Curry_ERD.C_Exactly x2) -> x2
     (Curry_ERD.C_Between x3 x4) -> d_OP__case_2 x4 x3250 x3500
     (Curry_ERD.Choice_C_Cardinality x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cardMaximum x1002 x3250 x3500) (d_C_cardMaximum x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Cardinality x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cardMaximum z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Cardinality x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cardMaximum x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Cardinality x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showERD :: Curry_Prelude.C_Int -> Curry_ERD.C_ERD -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showERD x1 x2 x3250 x3500 = case x2 of
     (Curry_ERD.C_ERD x3 x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_C_showString x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_lb x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_lb (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_map (d_C_showEs (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500)) x4 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_lb x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_lb (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_map (d_C_showRs (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3250 x3500)) x5 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_ERD.Choice_C_ERD x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showERD x1 x1002 x3250 x3500) (d_C_showERD x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_ERD x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showERD x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_ERD x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showERD x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_ERD x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showEs :: Curry_Prelude.C_Int -> Curry_ERD.C_Entity -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showEs x1 x2 x3250 x3500 = case x2 of
     (Curry_ERD.C_Entity x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showString x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_lb (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 7#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_lb (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 8#) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_map d_C_showWOBrackets x4 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_ERD.Choice_C_Entity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showEs x1 x1002 x3250 x3500) (d_C_showEs x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Entity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showEs x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Entity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showEs x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Entity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showRs :: Curry_Prelude.C_Int -> Curry_ERD.C_Relationship -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showRs x1 x2 x3250 x3500 = case x2 of
     (Curry_ERD.C_Relationship x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showString x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_lb (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 13#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (d_C_lb (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 14#) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_map d_C_showWOBrackets x4 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_ERD.Choice_C_Relationship x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showRs x1 x1002 x3250 x3500) (d_C_showRs x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Relationship x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showRs x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Relationship x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showRs x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Relationship x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showWOBrackets :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showWOBrackets x1 x3250 x3500 = d_OP_showWOBrackets_dot_stripBrackets_dot_114 (Curry_Prelude.d_C_show x1 x3250 x3500) x3250 x3500

d_OP_showWOBrackets_dot_stripBrackets_dot_114 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showWOBrackets_dot_stripBrackets_dot_114 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showWOBrackets_dot_stripBrackets_dot_114 x1002 x3250 x3500) (d_OP_showWOBrackets_dot_stripBrackets_dot_114 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showWOBrackets_dot_stripBrackets_dot_114 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showWOBrackets_dot_stripBrackets_dot_114 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showString x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500

d_C_lb :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_lb x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_take x1 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3250 x3500) x3250 x3500) x3250 x3500

d_C_combineIds :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_combineIds x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_combineIds_dot_maybeAddUnderscore_dot_122 x3250 x3500) x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineIds x1002 x3250 x3500) (d_C_combineIds x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineIds z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineIds x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_combineIds_dot_maybeAddUnderscore_dot_122 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_combineIds_dot_maybeAddUnderscore_dot_122 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_0 x2 x1 (Curry_Char.d_C_isUpper x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_combineIds_dot_maybeAddUnderscore_dot_122 x1002 x3250 x3500) (d_OP_combineIds_dot_maybeAddUnderscore_dot_122 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_combineIds_dot_maybeAddUnderscore_dot_122 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_combineIds_dot_maybeAddUnderscore_dot_122 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1 x1002 x3250 x3500) (d_OP__case_0 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_C_tail (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x2 x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x3 x1002 x3250 x3500) (d_OP__case_1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_ERD.C_MaxValue -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_2 x4 x3250 x3500 = case x4 of
     (Curry_ERD.C_Max x5) -> x5
     (Curry_ERD.Choice_C_MaxValue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_MaxValue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_MaxValue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_MaxValue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_ERD.C_Domain -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_3 x3 x3250 x3500 = case x3 of
     (Curry_ERD.C_KeyDom x6) -> Curry_Prelude.C_True
     (Curry_ERD.C_IntDom x7) -> Curry_Prelude.C_False
     (Curry_ERD.C_FloatDom x8) -> Curry_Prelude.C_False
     (Curry_ERD.C_CharDom x9) -> Curry_Prelude.C_False
     (Curry_ERD.C_StringDom x10) -> Curry_Prelude.C_False
     (Curry_ERD.C_BoolDom x11) -> Curry_Prelude.C_False
     (Curry_ERD.C_DateDom x12) -> Curry_Prelude.C_False
     (Curry_ERD.C_UserDefined x13 x14) -> Curry_Prelude.C_False
     (Curry_ERD.Choice_C_Domain x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3250 x3500) (d_OP__case_3 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Domain x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Domain x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Domain x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Domain -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_4 x1 x4 x3250 x3500 = case x4 of
     (Curry_ERD.C_KeyDom x7) -> Curry_Prelude.d_OP_eq_eq x7 x1 x3250 x3500
     (Curry_ERD.C_IntDom x8) -> Curry_Prelude.C_False
     (Curry_ERD.C_FloatDom x9) -> Curry_Prelude.C_False
     (Curry_ERD.C_CharDom x10) -> Curry_Prelude.C_False
     (Curry_ERD.C_StringDom x11) -> Curry_Prelude.C_False
     (Curry_ERD.C_BoolDom x12) -> Curry_Prelude.C_False
     (Curry_ERD.C_DateDom x13) -> Curry_Prelude.C_False
     (Curry_ERD.C_UserDefined x14 x15) -> Curry_Prelude.C_False
     (Curry_ERD.Choice_C_Domain x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x1002 x3250 x3500) (d_OP__case_4 x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Domain x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Domain x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Domain x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Domain -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_5 x1 x4 x3250 x3500 = case x4 of
     (Curry_ERD.C_KeyDom x7) -> Curry_Prelude.d_OP_eq_eq x7 x1 x3250 x3500
     (Curry_ERD.C_IntDom x8) -> Curry_Prelude.C_False
     (Curry_ERD.C_FloatDom x9) -> Curry_Prelude.C_False
     (Curry_ERD.C_CharDom x10) -> Curry_Prelude.C_False
     (Curry_ERD.C_StringDom x11) -> Curry_Prelude.C_False
     (Curry_ERD.C_BoolDom x12) -> Curry_Prelude.C_False
     (Curry_ERD.C_DateDom x13) -> Curry_Prelude.C_False
     (Curry_ERD.C_UserDefined x14 x15) -> Curry_Prelude.C_False
     (Curry_ERD.Choice_C_Domain x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x1002 x3250 x3500) (d_OP__case_5 x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Domain x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Domain x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Domain x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_ERD.C_Domain -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_6 x1 x4 x3250 x3500 = case x4 of
     (Curry_ERD.C_KeyDom x7) -> Curry_Prelude.d_OP_eq_eq x7 x1 x3250 x3500
     (Curry_ERD.C_IntDom x8) -> Curry_Prelude.C_False
     (Curry_ERD.C_FloatDom x9) -> Curry_Prelude.C_False
     (Curry_ERD.C_CharDom x10) -> Curry_Prelude.C_False
     (Curry_ERD.C_StringDom x11) -> Curry_Prelude.C_False
     (Curry_ERD.C_BoolDom x12) -> Curry_Prelude.C_False
     (Curry_ERD.C_DateDom x13) -> Curry_Prelude.C_False
     (Curry_ERD.C_UserDefined x14 x15) -> Curry_Prelude.C_False
     (Curry_ERD.Choice_C_Domain x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x1002 x3250 x3500) (d_OP__case_6 x1 x1003 x3250 x3500)
     (Curry_ERD.Choices_C_Domain x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 z x3250 x3500) x1002
     (Curry_ERD.Guard_C_Domain x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Domain x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
