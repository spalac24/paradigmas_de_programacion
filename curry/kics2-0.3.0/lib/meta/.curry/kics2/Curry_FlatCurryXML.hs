{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_FlatCurryXML (nd_C_flatCurry2XmlFile, nd_C_flatCurry2Xml, nd_C_xmlFile2FlatCurry, nd_C_xml2FlatCurry) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_Prelude
import qualified Curry_XML
import qualified Curry_XmlConv
d_C_flatCurryDtd :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_flatCurryDtd x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '~'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))

nd_C_flatCurry2XmlFile :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_flatCurry2XmlFile x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Prelude.d_C_writeFile x2)) (Curry_XML.d_C_showXmlDocWithParams (Curry_Prelude.OP_Cons (Curry_XML.C_DtdUrl (d_C_flatCurryDtd x3250 x3500)) Curry_Prelude.OP_List) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_flatCurry2Xml x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x3250 x3500) x2003 x3250 x3500)))))

nd_C_flatCurry2Xml :: IDSupply -> Cover -> ConstStore -> Func Curry_FlatCurry.C_Prog Curry_XML.C_XmlExp
nd_C_flatCurry2Xml x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (Curry_XmlConv.nd_C_xmlShow (nd_C_cProg x2000 x3250 x3500))))

nd_C_xmlFile2FlatCurry :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_C_xmlFile2FlatCurry x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_XML.d_C_readXmlFile x1 x3250 x3500) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (nd_C_xml2FlatCurry x2000 x3250 x3500) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

nd_C_xml2FlatCurry :: IDSupply -> Cover -> ConstStore -> Func Curry_XML.C_XmlExp Curry_FlatCurry.C_Prog
nd_C_xml2FlatCurry x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (Curry_XmlConv.nd_C_xmlRead (nd_C_cProg x2000 x3250 x3500))))

nd_C_cProg :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_Prog
nd_C_cProg x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2007 = leftSupply x2006
          x2009 = rightSupply x2006
           in (seq x2007 (seq x2009 (let
               x2005 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2005 (seq x2008 (let
                    x2000 = leftSupply x2008
                    x2001 = rightSupply x2008
                     in (seq x2000 (seq x2001 (let
                         x2002 = leftSupply x2009
                         x2010 = rightSupply x2009
                          in (seq x2002 (seq x2010 (let
                              x2003 = leftSupply x2010
                              x2004 = rightSupply x2010
                               in (seq x2003 (seq x2004 (Curry_XmlConv.nd_C_eSeq5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))) (wrapDX (wrapDX (wrapDX (wrapDX (wrapDX id)))) (acceptCs (acceptCs (acceptCs (acceptCs (acceptCs id)))) Curry_FlatCurry.C_Prog)) (nd_C_cModname x2000 x3250 x3500) (nd_C_cImports x2001 x3250 x3500) (nd_C_cTypes x2002 x3250 x3500) (nd_C_cFuncs x2003 x3250 x3500) (nd_C_cOps x2004 x3250 x3500) x2005 x3250 x3500)))))))))))))))))

nd_C_cModname :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_cModname x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_XmlConv.nd_C_eString (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) x2000 x3250 x3500))

nd_C_cImports :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_cImports x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_eRep (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) (Curry_XmlConv.nd_C_eString (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_cTypes :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl)
nd_C_cTypes x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_eRep (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (nd_C_cType x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_cType :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_TypeDecl
nd_C_cType x3000 x3250 x3500 = let
     x2021 = x3000
      in (seq x2021 (let
          x2020 = leftSupply x2021
          x2022 = rightSupply x2021
           in (seq x2020 (seq x2022 (let
               x2007 = leftSupply x2022
               x2016 = rightSupply x2022
                in (seq x2007 (seq x2016 (Curry_XmlConv.nd_OP_bang (let
                    x2008 = leftSupply x2007
                    x2009 = rightSupply x2007
                     in (seq x2008 (seq x2009 (let
                         x2006 = leftSupply x2008
                         x2000 = rightSupply x2008
                          in (seq x2006 (seq x2000 (let
                              x2001 = leftSupply x2009
                              x2010 = rightSupply x2009
                               in (seq x2001 (seq x2010 (let
                                   x2002 = leftSupply x2010
                                   x2005 = rightSupply x2010
                                    in (seq x2002 (seq x2005 (Curry_XmlConv.nd_C_eSeq4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs (acceptCs id))) Curry_FlatCurry.C_Type)) (nd_C_cQName x2000 x3250 x3500) (nd_C_cVis x2001 x3250 x3500) (nd_C_cTParams x2002 x3250 x3500) (let
                                        x2004 = leftSupply x2005
                                        x2003 = rightSupply x2005
                                         in (seq x2004 (seq x2003 (Curry_XmlConv.nd_C_rep (nd_C_cConsDecl x2003 x3250 x3500) x2004 x3250 x3500)))) x2006 x3250 x3500))))))))))))) (let
                    x2017 = leftSupply x2016
                    x2018 = rightSupply x2016
                     in (seq x2017 (seq x2018 (let
                         x2015 = leftSupply x2017
                         x2011 = rightSupply x2017
                          in (seq x2015 (seq x2011 (let
                              x2012 = leftSupply x2018
                              x2019 = rightSupply x2018
                               in (seq x2012 (seq x2019 (let
                                   x2013 = leftSupply x2019
                                   x2014 = rightSupply x2019
                                    in (seq x2013 (seq x2014 (Curry_XmlConv.nd_C_eSeq4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))) (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs (acceptCs id))) Curry_FlatCurry.C_TypeSyn)) (nd_C_cQName x2011 x3250 x3500) (nd_C_cVis x2012 x3250 x3500) (nd_C_cTParams x2013 x3250 x3500) (nd_C_cTypeExpr x2014 x3250 x3500) x2015 x3250 x3500))))))))))))) x2020 x3250 x3500))))))))

nd_C_cQName :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_cQName x3000 x3250 x3500 = let
     x2009 = x3000
      in (seq x2009 (let
          x2008 = leftSupply x2009
          x2010 = rightSupply x2009
           in (seq x2008 (seq x2010 (let
               x2005 = leftSupply x2010
               x2007 = rightSupply x2010
                in (seq x2005 (seq x2007 (Curry_Prelude.nd_C_apply (let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2002 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_XmlConv.nd_C_seq2 x2000 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id d_OP_cQName_dot___hash_lambda1)) x2001 x3250 x3500)))) (Curry_XmlConv.nd_C_aString (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) x2003 x3250 x3500) x2004 x3250 x3500))))))) (Curry_XmlConv.nd_C_aString (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x2007 x3250 x3500) x2008 x3250 x3500))))))))

d_OP_cQName_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_cQName_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.OP_Tuple2 x1 x2

d_C_cVis :: Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_FlatCurry.C_Visibility
d_C_cVis x3250 x3500 = Curry_XmlConv.d_C_adapt (Curry_Prelude.OP_Tuple2 d_C_b2v d_C_v2b) (Curry_XmlConv.d_C_aBool (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500

nd_C_cVis :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_FlatCurry.C_Visibility
nd_C_cVis x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_adapt (Curry_Prelude.OP_Tuple2 (wrapDX id d_C_b2v) (wrapDX id d_C_v2b)) (Curry_XmlConv.nd_C_aBool (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_b2v :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_FlatCurry.C_Visibility
d_C_b2v x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_FlatCurry.C_Public
     Curry_Prelude.C_False -> Curry_FlatCurry.C_Private
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_b2v x1002 x3250 x3500) (d_C_b2v x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_b2v z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_b2v x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_v2b :: Curry_FlatCurry.C_Visibility -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_v2b x1 x3250 x3500 = Curry_Prelude.d_OP_eq_eq x1 Curry_FlatCurry.C_Public x3250 x3500

nd_C_cTParams :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_List Curry_Prelude.C_Int)
nd_C_cTParams x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_eRep (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))) (Curry_XmlConv.nd_C_eInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))) x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_cConsDecl :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_ConsDecl
nd_C_cConsDecl x3000 x3250 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2008 = leftSupply x2007
          x2009 = rightSupply x2007
           in (seq x2008 (seq x2009 (let
               x2006 = leftSupply x2008
               x2000 = rightSupply x2008
                in (seq x2006 (seq x2000 (let
                    x2001 = leftSupply x2009
                    x2010 = rightSupply x2009
                     in (seq x2001 (seq x2010 (let
                         x2002 = leftSupply x2010
                         x2005 = rightSupply x2010
                          in (seq x2002 (seq x2005 (Curry_XmlConv.nd_C_eSeq4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))) (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs (acceptCs id))) Curry_FlatCurry.C_Cons)) (nd_C_cQName x2000 x3250 x3500) (nd_C_cArity x2001 x3250 x3500) (nd_C_cVis x2002 x3250 x3500) (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_XmlConv.nd_C_rep (nd_C_cTypeExpr x2003 x3250 x3500) x2004 x3250 x3500)))) x2006 x3250 x3500))))))))))))))

d_C_cArity :: Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_Prelude.C_Int
d_C_cArity x3250 x3500 = Curry_XmlConv.d_C_aInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x3250 x3500

nd_C_cArity :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_Prelude.C_Int
nd_C_cArity x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_XmlConv.nd_C_aInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x2000 x3250 x3500))

nd_C_cTypeExpr :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_TypeExpr
nd_C_cTypeExpr x3000 x3250 x3500 = let
     x2019 = x3000
      in (seq x2019 (let
          x2018 = leftSupply x2019
          x2020 = rightSupply x2019
           in (seq x2018 (seq x2020 (let
               x2003 = leftSupply x2020
               x2016 = rightSupply x2020
                in (seq x2003 (seq x2016 (Curry_XmlConv.nd_OP_bang (let
                    x2002 = leftSupply x2003
                    x2004 = rightSupply x2003
                     in (seq x2002 (seq x2004 (let
                         x2000 = leftSupply x2004
                         x2001 = rightSupply x2004
                          in (seq x2000 (seq x2001 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_FuncType)) (nd_C_cTypeExpr x2000 x3250 x3500) (nd_C_cTypeExpr x2001 x3250 x3500) x2002 x3250 x3500))))))) (let
                    x2015 = leftSupply x2016
                    x2017 = rightSupply x2016
                     in (seq x2015 (seq x2017 (let
                         x2010 = leftSupply x2017
                         x2014 = rightSupply x2017
                          in (seq x2010 (seq x2014 (Curry_XmlConv.nd_OP_bang (let
                              x2009 = leftSupply x2010
                              x2011 = rightSupply x2010
                               in (seq x2009 (seq x2011 (let
                                   x2005 = leftSupply x2011
                                   x2008 = rightSupply x2011
                                    in (seq x2005 (seq x2008 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_TCons)) (nd_C_cQName x2005 x3250 x3500) (let
                                        x2007 = leftSupply x2008
                                        x2006 = rightSupply x2008
                                         in (seq x2007 (seq x2006 (Curry_XmlConv.nd_C_rep (nd_C_cTypeExpr x2006 x3250 x3500) x2007 x3250 x3500)))) x2009 x3250 x3500))))))) (let
                              x2013 = leftSupply x2014
                              x2012 = rightSupply x2014
                               in (seq x2013 (seq x2012 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))) (wrapDX id (acceptCs id Curry_FlatCurry.C_TVar)) (Curry_XmlConv.nd_C_int x2012 x3250 x3500) x2013 x3250 x3500)))) x2015 x3250 x3500))))))) x2018 x3250 x3500))))))))

nd_C_cFuncs :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl)
nd_C_cFuncs x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_eRep (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) (nd_C_cFunc x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_cFunc :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_FuncDecl
nd_C_cFunc x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2007 = leftSupply x2006
          x2009 = rightSupply x2006
           in (seq x2007 (seq x2009 (let
               x2005 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2005 (seq x2008 (let
                    x2000 = leftSupply x2008
                    x2001 = rightSupply x2008
                     in (seq x2000 (seq x2001 (let
                         x2002 = leftSupply x2009
                         x2010 = rightSupply x2009
                          in (seq x2002 (seq x2010 (let
                              x2003 = leftSupply x2010
                              x2004 = rightSupply x2010
                               in (seq x2003 (seq x2004 (Curry_XmlConv.nd_C_eSeq5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))) (wrapDX (wrapDX (wrapDX (wrapDX (wrapDX id)))) (acceptCs (acceptCs (acceptCs (acceptCs (acceptCs id)))) Curry_FlatCurry.C_Func)) (nd_C_cQName x2000 x3250 x3500) (nd_C_cArity x2001 x3250 x3500) (nd_C_cVis x2002 x3250 x3500) (nd_C_cTypeExpr x2003 x3250 x3500) (nd_C_cRule x2004 x3250 x3500) x2005 x3250 x3500)))))))))))))))))

nd_C_cRule :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_Rule
nd_C_cRule x3000 x3250 x3500 = let
     x2009 = x3000
      in (seq x2009 (let
          x2008 = leftSupply x2009
          x2010 = rightSupply x2009
           in (seq x2008 (seq x2010 (let
               x2003 = leftSupply x2010
               x2007 = rightSupply x2010
                in (seq x2003 (seq x2007 (Curry_XmlConv.nd_OP_bang (let
                    x2002 = leftSupply x2003
                    x2004 = rightSupply x2003
                     in (seq x2002 (seq x2004 (let
                         x2000 = leftSupply x2004
                         x2001 = rightSupply x2004
                          in (seq x2000 (seq x2001 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_Rule)) (nd_C_cLHS x2000 x3250 x3500) (nd_C_cRHS x2001 x3250 x3500) x2002 x3250 x3500))))))) (let
                    x2006 = leftSupply x2007
                    x2005 = rightSupply x2007
                     in (seq x2006 (seq x2005 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))) (wrapDX id (acceptCs id Curry_FlatCurry.C_External)) (Curry_XmlConv.nd_C_string x2005 x3250 x3500) x2006 x3250 x3500)))) x2008 x3250 x3500))))))))

nd_C_cLHS :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_List Curry_Prelude.C_Int)
nd_C_cLHS x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_element (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) (nd_C_cVars x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_cRHS :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_Expr
nd_C_cRHS x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_element (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))) (nd_C_cExpr x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_cVars :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem (Curry_Prelude.OP_List Curry_Prelude.C_Int)
nd_C_cVars x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_rep (nd_C_cVar x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_cVar :: Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_Prelude.C_Int
d_C_cVar x3250 x3500 = Curry_XmlConv.d_C_eInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) x3250 x3500

nd_C_cVar :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_Prelude.C_Int
nd_C_cVar x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_XmlConv.nd_C_eInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) x2000 x3250 x3500))

nd_C_cExpr :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_Expr
nd_C_cExpr x3000 x3250 x3500 = let
     x2107 = x3000
      in (seq x2107 (let
          x2106 = leftSupply x2107
          x2108 = rightSupply x2107
           in (seq x2106 (seq x2108 (let
               x2002 = leftSupply x2108
               x2104 = rightSupply x2108
                in (seq x2002 (seq x2104 (Curry_XmlConv.nd_OP_bang (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (wrapDX id (acceptCs id Curry_FlatCurry.C_Var)) (Curry_XmlConv.nd_C_int x2000 x3250 x3500) x2001 x3250 x3500)))) (let
                    x2103 = leftSupply x2104
                    x2105 = rightSupply x2104
                     in (seq x2103 (seq x2105 (let
                         x2005 = leftSupply x2105
                         x2101 = rightSupply x2105
                          in (seq x2005 (seq x2101 (Curry_XmlConv.nd_OP_bang (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))) (wrapDX id (acceptCs id Curry_FlatCurry.C_Lit)) (nd_C_cLit x2003 x3250 x3500) x2004 x3250 x3500)))) (let
                              x2100 = leftSupply x2101
                              x2102 = rightSupply x2101
                               in (seq x2100 (seq x2102 (let
                                   x2010 = leftSupply x2102
                                   x2098 = rightSupply x2102
                                    in (seq x2010 (seq x2098 (Curry_XmlConv.nd_OP_bang (let
                                        x2011 = leftSupply x2010
                                        x2012 = rightSupply x2010
                                         in (seq x2011 (seq x2012 (let
                                             x2009 = leftSupply x2011
                                             x2006 = rightSupply x2011
                                              in (seq x2009 (seq x2006 (let
                                                  x2007 = leftSupply x2012
                                                  x2008 = rightSupply x2012
                                                   in (seq x2007 (seq x2008 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))) (nd_C_fc x2006 x3250 x3500) (nd_C_cQName x2007 x3250 x3500) (nd_C_cExps x2008 x3250 x3500) x2009 x3250 x3500)))))))))) (let
                                        x2097 = leftSupply x2098
                                        x2099 = rightSupply x2098
                                         in (seq x2097 (seq x2099 (let
                                             x2017 = leftSupply x2099
                                             x2095 = rightSupply x2099
                                              in (seq x2017 (seq x2095 (Curry_XmlConv.nd_OP_bang (let
                                                  x2018 = leftSupply x2017
                                                  x2019 = rightSupply x2017
                                                   in (seq x2018 (seq x2019 (let
                                                       x2016 = leftSupply x2018
                                                       x2013 = rightSupply x2018
                                                        in (seq x2016 (seq x2013 (let
                                                            x2014 = leftSupply x2019
                                                            x2015 = rightSupply x2019
                                                             in (seq x2014 (seq x2015 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))) (nd_C_cc x2013 x3250 x3500) (nd_C_cQName x2014 x3250 x3500) (nd_C_cExps x2015 x3250 x3500) x2016 x3250 x3500)))))))))) (let
                                                  x2094 = leftSupply x2095
                                                  x2096 = rightSupply x2095
                                                   in (seq x2094 (seq x2096 (let
                                                       x2024 = leftSupply x2096
                                                       x2092 = rightSupply x2096
                                                        in (seq x2024 (seq x2092 (Curry_XmlConv.nd_OP_bang (let
                                                            x2025 = leftSupply x2024
                                                            x2026 = rightSupply x2024
                                                             in (seq x2025 (seq x2026 (let
                                                                 x2023 = leftSupply x2025
                                                                 x2020 = rightSupply x2025
                                                                  in (seq x2023 (seq x2020 (let
                                                                      x2021 = leftSupply x2026
                                                                      x2022 = rightSupply x2026
                                                                       in (seq x2021 (seq x2022 (Curry_XmlConv.nd_C_eSeq3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))))))) (wrapDX (wrapNX id) (acceptCs id nd_C_pfc)) (nd_C_cQName x2020 x3250 x3500) (nd_C_cMissing x2021 x3250 x3500) (nd_C_cExps x2022 x3250 x3500) x2023 x3250 x3500)))))))))) (let
                                                            x2091 = leftSupply x2092
                                                            x2093 = rightSupply x2092
                                                             in (seq x2091 (seq x2093 (let
                                                                 x2031 = leftSupply x2093
                                                                 x2089 = rightSupply x2093
                                                                  in (seq x2031 (seq x2089 (Curry_XmlConv.nd_OP_bang (let
                                                                      x2032 = leftSupply x2031
                                                                      x2033 = rightSupply x2031
                                                                       in (seq x2032 (seq x2033 (let
                                                                           x2030 = leftSupply x2032
                                                                           x2027 = rightSupply x2032
                                                                            in (seq x2030 (seq x2027 (let
                                                                                x2028 = leftSupply x2033
                                                                                x2029 = rightSupply x2033
                                                                                 in (seq x2028 (seq x2029 (Curry_XmlConv.nd_C_eSeq3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))))))) (wrapDX (wrapNX id) (acceptCs id nd_C_pcc)) (nd_C_cQName x2027 x3250 x3500) (nd_C_cMissing x2028 x3250 x3500) (nd_C_cExps x2029 x3250 x3500) x2030 x3250 x3500)))))))))) (let
                                                                      x2088 = leftSupply x2089
                                                                      x2090 = rightSupply x2089
                                                                       in (seq x2088 (seq x2090 (let
                                                                           x2039 = leftSupply x2090
                                                                           x2086 = rightSupply x2090
                                                                            in (seq x2039 (seq x2086 (Curry_XmlConv.nd_OP_bang (let
                                                                                x2038 = leftSupply x2039
                                                                                x2040 = rightSupply x2039
                                                                                 in (seq x2038 (seq x2040 (let
                                                                                     x2036 = leftSupply x2040
                                                                                     x2037 = rightSupply x2040
                                                                                      in (seq x2036 (seq x2037 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_Free)) (let
                                                                                          x2035 = leftSupply x2036
                                                                                          x2034 = rightSupply x2036
                                                                                           in (seq x2035 (seq x2034 (Curry_XmlConv.nd_C_element (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))) (nd_C_cVars x2034 x3250 x3500) x2035 x3250 x3500)))) (nd_C_cExpr x2037 x3250 x3500) x2038 x3250 x3500))))))) (let
                                                                                x2085 = leftSupply x2086
                                                                                x2087 = rightSupply x2086
                                                                                 in (seq x2085 (seq x2087 (let
                                                                                     x2044 = leftSupply x2087
                                                                                     x2083 = rightSupply x2087
                                                                                      in (seq x2044 (seq x2083 (Curry_XmlConv.nd_OP_bang (let
                                                                                          x2043 = leftSupply x2044
                                                                                          x2045 = rightSupply x2044
                                                                                           in (seq x2043 (seq x2045 (let
                                                                                               x2041 = leftSupply x2045
                                                                                               x2042 = rightSupply x2045
                                                                                                in (seq x2041 (seq x2042 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_Or)) (nd_C_cExpr x2041 x3250 x3500) (nd_C_cExpr x2042 x3250 x3500) x2043 x3250 x3500))))))) (let
                                                                                          x2082 = leftSupply x2083
                                                                                          x2084 = rightSupply x2083
                                                                                           in (seq x2082 (seq x2084 (let
                                                                                               x2052 = leftSupply x2084
                                                                                               x2080 = rightSupply x2084
                                                                                                in (seq x2052 (seq x2080 (Curry_XmlConv.nd_OP_bang (let
                                                                                                    x2053 = leftSupply x2052
                                                                                                    x2054 = rightSupply x2052
                                                                                                     in (seq x2053 (seq x2054 (let
                                                                                                         x2051 = leftSupply x2053
                                                                                                         x2046 = rightSupply x2053
                                                                                                          in (seq x2051 (seq x2046 (let
                                                                                                              x2047 = leftSupply x2054
                                                                                                              x2050 = rightSupply x2054
                                                                                                               in (seq x2047 (seq x2050 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (nd_C_cr x2046 x3250 x3500) (nd_C_cExpr x2047 x3250 x3500) (let
                                                                                                                   x2049 = leftSupply x2050
                                                                                                                   x2048 = rightSupply x2050
                                                                                                                    in (seq x2049 (seq x2048 (Curry_XmlConv.nd_C_rep (nd_C_cBranch x2048 x3250 x3500) x2049 x3250 x3500)))) x2051 x3250 x3500)))))))))) (let
                                                                                                    x2079 = leftSupply x2080
                                                                                                    x2081 = rightSupply x2080
                                                                                                     in (seq x2079 (seq x2081 (let
                                                                                                         x2061 = leftSupply x2081
                                                                                                         x2077 = rightSupply x2081
                                                                                                          in (seq x2061 (seq x2077 (Curry_XmlConv.nd_OP_bang (let
                                                                                                              x2062 = leftSupply x2061
                                                                                                              x2063 = rightSupply x2061
                                                                                                               in (seq x2062 (seq x2063 (let
                                                                                                                   x2060 = leftSupply x2062
                                                                                                                   x2055 = rightSupply x2062
                                                                                                                    in (seq x2060 (seq x2055 (let
                                                                                                                        x2056 = leftSupply x2063
                                                                                                                        x2059 = rightSupply x2063
                                                                                                                         in (seq x2056 (seq x2059 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (nd_C_cf x2055 x3250 x3500) (nd_C_cExpr x2056 x3250 x3500) (let
                                                                                                                             x2058 = leftSupply x2059
                                                                                                                             x2057 = rightSupply x2059
                                                                                                                              in (seq x2058 (seq x2057 (Curry_XmlConv.nd_C_rep (nd_C_cBranch x2057 x3250 x3500) x2058 x3250 x3500)))) x2060 x3250 x3500)))))))))) (let
                                                                                                              x2076 = leftSupply x2077
                                                                                                              x2078 = rightSupply x2077
                                                                                                               in (seq x2076 (seq x2078 (let
                                                                                                                   x2069 = leftSupply x2078
                                                                                                                   x2074 = rightSupply x2078
                                                                                                                    in (seq x2069 (seq x2074 (Curry_XmlConv.nd_OP_bang (let
                                                                                                                        x2068 = leftSupply x2069
                                                                                                                        x2070 = rightSupply x2069
                                                                                                                         in (seq x2068 (seq x2070 (let
                                                                                                                             x2066 = leftSupply x2070
                                                                                                                             x2067 = rightSupply x2070
                                                                                                                              in (seq x2066 (seq x2067 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_Let)) (let
                                                                                                                                  x2065 = leftSupply x2066
                                                                                                                                  x2064 = rightSupply x2066
                                                                                                                                   in (seq x2065 (seq x2064 (Curry_XmlConv.nd_C_rep (nd_C_cBind x2064 x3250 x3500) x2065 x3250 x3500)))) (nd_C_cExpr x2067 x3250 x3500) x2068 x3250 x3500))))))) (let
                                                                                                                        x2073 = leftSupply x2074
                                                                                                                        x2075 = rightSupply x2074
                                                                                                                         in (seq x2073 (seq x2075 (let
                                                                                                                             x2071 = leftSupply x2075
                                                                                                                             x2072 = rightSupply x2075
                                                                                                                              in (seq x2071 (seq x2072 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_Typed)) (nd_C_cExpr x2071 x3250 x3500) (nd_C_cTypeExpr x2072 x3250 x3500) x2073 x3250 x3500))))))) x2076 x3250 x3500))))))) x2079 x3250 x3500))))))) x2082 x3250 x3500))))))) x2085 x3250 x3500))))))) x2088 x3250 x3500))))))) x2091 x3250 x3500))))))) x2094 x3250 x3500))))))) x2097 x3250 x3500))))))) x2100 x3250 x3500))))))) x2103 x3250 x3500))))))) x2106 x3250 x3500))))))))

nd_C_cLit :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_Literal
nd_C_cLit x3000 x3250 x3500 = let
     x2015 = x3000
      in (seq x2015 (let
          x2014 = leftSupply x2015
          x2016 = rightSupply x2015
           in (seq x2014 (seq x2016 (let
               x2002 = leftSupply x2016
               x2012 = rightSupply x2016
                in (seq x2002 (seq x2012 (Curry_XmlConv.nd_OP_bang (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))) (wrapDX id (acceptCs id Curry_FlatCurry.C_Intc)) (Curry_XmlConv.nd_C_int x2000 x3250 x3500) x2001 x3250 x3500)))) (let
                    x2011 = leftSupply x2012
                    x2013 = rightSupply x2012
                     in (seq x2011 (seq x2013 (let
                         x2005 = leftSupply x2013
                         x2010 = rightSupply x2013
                          in (seq x2005 (seq x2010 (Curry_XmlConv.nd_OP_bang (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))) (wrapDX id (acceptCs id Curry_FlatCurry.C_Floatc)) (Curry_XmlConv.nd_C_float x2003 x3250 x3500) x2004 x3250 x3500)))) (let
                              x2009 = leftSupply x2010
                              x2008 = rightSupply x2010
                               in (seq x2009 (seq x2008 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))) (wrapDX id (acceptCs id Curry_FlatCurry.C_Charc)) (let
                                   x2007 = leftSupply x2008
                                   x2006 = rightSupply x2008
                                    in (seq x2007 (seq x2006 (Curry_XmlConv.nd_C_adapt (Curry_Prelude.OP_Tuple2 (wrapDX id Curry_Prelude.d_C_chr) (wrapDX id Curry_Prelude.d_C_ord)) (Curry_XmlConv.nd_C_int x2006 x3250 x3500) x2007 x3250 x3500)))) x2009 x3250 x3500)))) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))))

d_C_fc :: Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_fc x3250 x3500 = acceptCs (acceptCs id) (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_FuncCall)

nd_C_fc :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) Curry_FlatCurry.C_Expr)
nd_C_fc x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs (acceptCs id) (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_FuncCall))

d_C_cc :: Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_cc x3250 x3500 = acceptCs (acceptCs id) (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_ConsCall)

nd_C_cc :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) Curry_FlatCurry.C_Expr)
nd_C_cc x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs (acceptCs id) (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_ConsCall))

d_C_pfc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_pfc x1 x2 x3250 x3500 = acceptCs id (Curry_FlatCurry.C_Comb (Curry_FlatCurry.C_FuncPartCall x2) x1)

nd_C_pfc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) Curry_FlatCurry.C_Expr
nd_C_pfc x1 x2 x3000 x3250 x3500 = wrapDX id (acceptCs id (Curry_FlatCurry.C_Comb (Curry_FlatCurry.C_FuncPartCall x2) x1))

d_C_pcc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_pcc x1 x2 x3250 x3500 = acceptCs id (Curry_FlatCurry.C_Comb (Curry_FlatCurry.C_ConsPartCall x2) x1)

nd_C_pcc :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr) Curry_FlatCurry.C_Expr
nd_C_pcc x1 x2 x3000 x3250 x3500 = wrapDX id (acceptCs id (Curry_FlatCurry.C_Comb (Curry_FlatCurry.C_ConsPartCall x2) x1))

nd_C_cExps :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem (Curry_Prelude.OP_List Curry_FlatCurry.C_Expr)
nd_C_cExps x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_rep (nd_C_cExpr x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_cMissing :: Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_Prelude.C_Int
d_C_cMissing x3250 x3500 = Curry_XmlConv.d_C_aInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))) x3250 x3500

nd_C_cMissing :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_Prelude.C_Int
nd_C_cMissing x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_XmlConv.nd_C_aInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))) x2000 x3250 x3500))

d_C_cr :: Cover -> ConstStore -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_cr x3250 x3500 = acceptCs (acceptCs id) (Curry_FlatCurry.C_Case Curry_FlatCurry.C_Rigid)

nd_C_cr :: IDSupply -> Cover -> ConstStore -> Func Curry_FlatCurry.C_Expr (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr) Curry_FlatCurry.C_Expr)
nd_C_cr x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs (acceptCs id) (Curry_FlatCurry.C_Case Curry_FlatCurry.C_Rigid))

d_C_cf :: Cover -> ConstStore -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_cf x3250 x3500 = acceptCs (acceptCs id) (Curry_FlatCurry.C_Case Curry_FlatCurry.C_Flex)

nd_C_cf :: IDSupply -> Cover -> ConstStore -> Func Curry_FlatCurry.C_Expr (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr) Curry_FlatCurry.C_Expr)
nd_C_cf x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs (acceptCs id) (Curry_FlatCurry.C_Case Curry_FlatCurry.C_Flex))

nd_C_cBranch :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_BranchExpr
nd_C_cBranch x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) Curry_Prelude.OP_List)))))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_Branch)) (nd_C_cPat x2000 x3250 x3500) (nd_C_cExpr x2001 x3250 x3500) x2002 x3250 x3500))))))))

nd_C_cPat :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_Pattern
nd_C_cPat x3000 x3250 x3500 = let
     x2009 = x3000
      in (seq x2009 (let
          x2008 = leftSupply x2009
          x2010 = rightSupply x2009
           in (seq x2008 (seq x2010 (let
               x2003 = leftSupply x2010
               x2007 = rightSupply x2010
                in (seq x2003 (seq x2007 (Curry_XmlConv.nd_OP_bang (let
                    x2002 = leftSupply x2003
                    x2004 = rightSupply x2003
                     in (seq x2002 (seq x2004 (let
                         x2000 = leftSupply x2004
                         x2001 = rightSupply x2004
                          in (seq x2000 (seq x2001 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))) (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_FlatCurry.C_Pattern)) (nd_C_cQName x2000 x3250 x3500) (nd_C_cVars x2001 x3250 x3500) x2002 x3250 x3500))))))) (let
                    x2006 = leftSupply x2007
                    x2005 = rightSupply x2007
                     in (seq x2006 (seq x2005 (Curry_XmlConv.nd_C_eSeq1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))) (wrapDX id (acceptCs id Curry_FlatCurry.C_LPattern)) (nd_C_cLit x2005 x3250 x3500) x2006 x3250 x3500)))) x2008 x3250 x3500))))))))

nd_C_cBind :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr)
nd_C_cBind x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_XmlConv.nd_C_eSeq2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))) (wrapDX (wrapDX id) (acceptCs id d_OP_cBind_dot___hash_lambda2)) (nd_C_cVar x2000 x3250 x3500) (nd_C_cExpr x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_OP_cBind_dot___hash_lambda2 :: Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr
d_OP_cBind_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.OP_Tuple2 x1 x2

nd_C_cOps :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl)
nd_C_cOps x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_eRep (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) (nd_C_cOp x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_cOp :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_Repeatable Curry_XmlConv.C_Elem Curry_FlatCurry.C_OpDecl
nd_C_cOp x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2005 = leftSupply x2004
          x2006 = rightSupply x2004
           in (seq x2005 (seq x2006 (let
               x2003 = leftSupply x2005
               x2000 = rightSupply x2005
                in (seq x2003 (seq x2000 (let
                    x2001 = leftSupply x2006
                    x2002 = rightSupply x2006
                     in (seq x2001 (seq x2002 (Curry_XmlConv.nd_C_eSeq3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)) (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs (acceptCs id)) Curry_FlatCurry.C_Op)) (nd_C_cQName x2000 x3250 x3500) (nd_C_cFixity x2001 x3250 x3500) (Curry_XmlConv.nd_C_aInt (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))) x2002 x3250 x3500) x2003 x3250 x3500)))))))))))

d_C_cFixity :: Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_FlatCurry.C_Fixity
d_C_cFixity x3250 x3500 = Curry_XmlConv.d_C_adapt (Curry_Prelude.OP_Tuple2 d_C_rf Curry_Prelude.d_C_show) (Curry_XmlConv.d_C_aString (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500

nd_C_cFixity :: IDSupply -> Cover -> ConstStore -> Curry_XmlConv.C_XmlConv Curry_XmlConv.C_NotRepeatable Curry_XmlConv.C_NoElem Curry_FlatCurry.C_Fixity
nd_C_cFixity x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_XmlConv.nd_C_adapt (Curry_Prelude.OP_Tuple2 (wrapDX id d_C_rf) (wrapDX id Curry_Prelude.d_C_show)) (Curry_XmlConv.nd_C_aString (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_rf :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_C_rf x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_23 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_rf x1002 x3250 x3500) (d_C_rf x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_rf z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_rf x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_23 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.C_Char 'I'#) -> d_OP__case_22 x3 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('I',d_OP__case_22 x3 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x3 x1002 x3250 x3500) (d_OP__case_23 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_22 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_21 x5 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1002 x3250 x3500) (d_OP__case_22 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_21 x5 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.C_Char 'n'#) -> d_OP__case_20 x5 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',d_OP__case_20 x5 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x5 x1002 x3250 x3500) (d_OP__case_21 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_20 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_19 x7 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1002 x3250 x3500) (d_OP__case_20 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_19 x7 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.C_Char 'f'#) -> d_OP__case_18 x7 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('f',d_OP__case_18 x7 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x7 x1002 x3250 x3500) (d_OP__case_19 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_18 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_17 x9 x8 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1002 x3250 x3500) (d_OP__case_18 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_17 x9 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_16 x9 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_16 x9 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x9 x1002 x3250 x3500) (d_OP__case_17 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_16 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_15 x11 x10 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1002 x3250 x3500) (d_OP__case_16 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_15 x11 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.C_Char 'x'#) -> d_OP__case_14 x11 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('x',d_OP__case_14 x11 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x11 x1002 x3250 x3500) (d_OP__case_15 x11 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x11 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_14 x11 x3250 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_13 x13 x12 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1002 x3250 x3500) (d_OP__case_14 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_13 x13 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.C_Char 'O'#) -> d_OP__case_12 x13 x3250 x3500
     (Curry_Prelude.C_Char 'l'#) -> d_OP__case_9 x13 x3250 x3500
     (Curry_Prelude.C_Char 'r'#) -> d_OP__case_4 x13 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('O',d_OP__case_12 x13 x3250 x3500),('l',d_OP__case_9 x13 x3250 x3500),('r',d_OP__case_4 x13 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x13 x1002 x3250 x3500) (d_OP__case_13 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_4 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x20 x21) -> d_OP__case_3 x21 x20 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1002 x3250 x3500) (d_OP__case_4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_3 x21 x20 x3250 x3500 = case x20 of
     (Curry_Prelude.C_Char 'O'#) -> d_OP__case_2 x21 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('O',d_OP__case_2 x21 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x21 x1002 x3250 x3500) (d_OP__case_3 x21 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x21 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x21 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_2 x21 x3250 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x22 x23) -> d_OP__case_1 x23 x22 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_1 x23 x22 x3250 x3500 = case x22 of
     (Curry_Prelude.C_Char 'p'#) -> d_OP__case_0 x23 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('p',d_OP__case_0 x23 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x23 x1002 x3250 x3500) (d_OP__case_1 x23 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x23 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x23 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_0 x23 x3250 x3500 = case x23 of
     Curry_Prelude.OP_List -> Curry_FlatCurry.C_InfixrOp
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3250 x3500) (d_OP__case_0 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_9 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x16 x17) -> d_OP__case_8 x17 x16 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1002 x3250 x3500) (d_OP__case_9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_8 x17 x16 x3250 x3500 = case x16 of
     (Curry_Prelude.C_Char 'O'#) -> d_OP__case_7 x17 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('O',d_OP__case_7 x17 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x17 x1002 x3250 x3500) (d_OP__case_8 x17 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x17 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x17 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_7 x17 x3250 x3500 = case x17 of
     (Curry_Prelude.OP_Cons x18 x19) -> d_OP__case_6 x19 x18 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_6 x19 x18 x3250 x3500 = case x18 of
     (Curry_Prelude.C_Char 'p'#) -> d_OP__case_5 x19 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('p',d_OP__case_5 x19 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x19 x1002 x3250 x3500) (d_OP__case_6 x19 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x19 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x19 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_5 x19 x3250 x3500 = case x19 of
     Curry_Prelude.OP_List -> Curry_FlatCurry.C_InfixlOp
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1002 x3250 x3500) (d_OP__case_5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_12 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x14 x15) -> d_OP__case_11 x15 x14 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1002 x3250 x3500) (d_OP__case_12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_11 x15 x14 x3250 x3500 = case x14 of
     (Curry_Prelude.C_Char 'p'#) -> d_OP__case_10 x15 x3250 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('p',d_OP__case_10 x15 x3250 x3500)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x15 x1002 x3250 x3500) (d_OP__case_11 x15 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x15 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x15 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_FlatCurry.C_Fixity
d_OP__case_10 x15 x3250 x3500 = case x15 of
     Curry_Prelude.OP_List -> Curry_FlatCurry.C_InfixOp
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1002 x3250 x3500) (d_OP__case_10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
