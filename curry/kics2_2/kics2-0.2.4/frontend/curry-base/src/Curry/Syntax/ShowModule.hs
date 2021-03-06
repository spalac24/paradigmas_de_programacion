{- |
    Module      :  $Header$
    Copyright   :  (c) Sebastian Fischer 2008
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    Transform a CurrySyntax module into a string representation without any
    pretty printing.

    Behaves like a derived Show instance even on parts with a specific one.
-}
module Curry.Syntax.ShowModule (showModule) where

import Curry.Base.Ident
import Curry.Base.Position

import Curry.Syntax.Type

-- |Show a Curry module like by an devired 'Show' instance
showModule :: Module -> String
showModule m = showsModule m "\n"

showsModule :: Module -> ShowS
showsModule (Module mident espec imps decls)
  = showsString "Module "
  . showsModuleIdent mident . newline
  . showsMaybe showsExportSpec espec . newline
  . showsList (\i -> showsImportDecl i . newline) imps
  . showsList (\d -> showsDecl d . newline) decls

showsExportSpec :: ExportSpec -> ShowS
showsExportSpec (Exporting pos exports)
  = showsString "(Exporting "
  . showsPosition pos . space
  . showsList showsExport exports
  . showsString ")"

showsExport :: Export -> ShowS
showsExport (Export qident)
  = showsString "(Export "
  . showsQualIdent qident
  . showsString ")"
showsExport (ExportTypeWith qident ids)
  = showsString "(ExportTypeWith "
  . showsQualIdent qident . space
  . showsList showsIdent ids
  . showsString ")"
showsExport (ExportTypeAll qident)
  = showsString "(ExportTypeAll "
  . showsQualIdent qident
  . showsString ")"
showsExport (ExportModule m)
  = showsString "(ExportModule "
  . showsModuleIdent m
  . showsString ")"

showsImportDecl :: ImportDecl -> ShowS
showsImportDecl (ImportDecl pos mident quali mmident mimpspec)
  = showsString "(ImportDecl "
  . showsPosition pos . space
  . showsModuleIdent mident . space
  . shows quali . space
  . showsMaybe showsModuleIdent mmident . space
  . showsMaybe showsImportSpec mimpspec
  . showsString ")"

showsImportSpec :: ImportSpec -> ShowS
showsImportSpec (Importing pos imports)
  = showsString "(Importing "
  . showsPosition pos . space
  . showsList showsImport imports
  . showsString ")"
showsImportSpec (Hiding pos imports)
  = showsString "(Hiding "
  . showsPosition pos . space
  . showsList showsImport imports
  . showsString ")"

showsImport :: Import -> ShowS
showsImport (Import ident)
  = showsString "(Import "
  . showsIdent ident
  . showsString ")"
showsImport (ImportTypeWith ident idents)
  = showsString "(ImportTypeWith "
  . showsIdent ident . space
  . showsList showsIdent idents
  . showsString ")"
showsImport (ImportTypeAll ident)
  = showsString "(ImportTypeAll "
  . showsIdent ident
  . showsString ")"

showsDecl :: Decl -> ShowS
showsDecl (InfixDecl pos infx prec idents)
  = showsString "(InfixDecl "
  . showsPosition pos . space
  . shows infx . space
  . shows prec . space
  . showsList showsIdent idents
  . showsString ")"
showsDecl (DataDecl pos ident idents consdecls)
  = showsString "(DataDecl "
  . showsPosition pos . space
  . showsIdent ident . space
  . showsList showsIdent idents . space
  . showsList showsConsDecl consdecls
  . showsString ")"
showsDecl (NewtypeDecl pos ident idents newconsdecl)
  = showsString "(NewtypeDecl "
  . showsPosition pos . space
  . showsIdent ident . space
  . showsList showsIdent idents . space
  . showsNewConsDecl newconsdecl
  . showsString ")"
showsDecl (TypeDecl pos ident idents typ)
  = showsString "(TypeDecl "
  . showsPosition pos . space
  . showsIdent ident . space
  . showsList showsIdent idents . space
  . showsTypeExpr typ
  . showsString ")"
showsDecl (TypeSig pos idents typ)
  = showsString "(TypeSig "
  . showsPosition pos . space
  . showsList showsIdent idents . space
  . showsTypeExpr typ
  . showsString ")"
showsDecl (FunctionDecl pos ident eqs)
  = showsString "(FunctionDecl "
  . showsPosition pos . space
  . showsIdent ident . space
  . showsList showsEquation eqs
  . showsString ")"
showsDecl (ForeignDecl pos cconv mstr ident typ)
  = showsString "(ForeignDecl "
  . showsPosition pos . space
  . shows cconv . space
  . shows mstr . space
  . showsIdent ident . space
  . showsTypeExpr typ
  . showsString ")"
showsDecl (ExternalDecl pos idents)
  = showsString "(ExternalDecl "
  . showsPosition pos . space
  . showsList showsIdent idents
  . showsString ")"
showsDecl (PatternDecl pos cons rhs)
  = showsString "(PatternDecl "
  . showsPosition pos . space
  . showsConsTerm cons . space
  . showsRhs rhs
  . showsString ")"
showsDecl (FreeDecl pos idents)
  = showsString "(FreeDecl "
  . showsPosition pos . space
  . showsList showsIdent idents
  . showsString ")"

showsConsDecl :: ConstrDecl -> ShowS
showsConsDecl (ConstrDecl pos idents ident types)
  = showsString "(ConstrDecl "
  . showsPosition pos . space
  . showsList showsIdent idents . space
  . showsIdent ident . space
  . showsList showsTypeExpr types
  . showsString ")"
showsConsDecl (ConOpDecl pos idents ty1 ident ty2)
  = showsString "(ConOpDecl "
  . showsPosition pos . space
  . showsList showsIdent idents . space
  . showsTypeExpr ty1 . space
  . showsIdent ident . space
  . showsTypeExpr ty2
  . showsString ")"

showsNewConsDecl :: NewConstrDecl -> ShowS
showsNewConsDecl (NewConstrDecl pos idents ident typ)
  = showsString "(NewConstrDecl "
  . showsPosition pos . space
  . showsList showsIdent idents . space
  . showsIdent ident . space
  . showsTypeExpr typ
  . showsString ")"

showsTypeExpr :: TypeExpr -> ShowS
showsTypeExpr (ConstructorType qident types)
  = showsString "(ConstructorType "
  . showsQualIdent qident . space
  . showsList showsTypeExpr types
  . showsString ")"
showsTypeExpr (VariableType ident)
  = showsString "(VariableType "
  . showsIdent ident
  . showsString ")"
showsTypeExpr (TupleType types)
  = showsString "(TupleType "
  . showsList showsTypeExpr types
  . showsString ")"
showsTypeExpr (ListType typ)
  = showsString "(ListType "
  . showsTypeExpr typ
  . showsString ")"
showsTypeExpr (ArrowType dom ran)
  = showsString "(ArrowType "
  . showsTypeExpr dom . space
  . showsTypeExpr ran
  . showsString ")"
showsTypeExpr (RecordType fieldts mtyp)
  = showsString "(RecordType "
  . showsList (showsPair (showsList showsIdent) showsTypeExpr) fieldts . space
  . showsMaybe showsTypeExpr mtyp
  . showsString ")"

showsEquation :: Equation -> ShowS
showsEquation (Equation pos lhs rhs)
  = showsString "(Equation "
  . showsPosition pos . space
  . showsLhs lhs . space
  . showsRhs rhs
  . showsString ")"

showsLhs :: Lhs -> ShowS
showsLhs (FunLhs ident conss)
  = showsString "(FunLhs "
  . showsIdent ident . space
  . showsList showsConsTerm conss
  . showsString ")"
showsLhs (OpLhs cons1 ident cons2)
  = showsString "(OpLhs "
  . showsConsTerm cons1 . space
  . showsIdent ident . space
  . showsConsTerm cons2
  . showsString ")"
showsLhs (ApLhs lhs conss)
  = showsString "(ApLhs "
  . showsLhs lhs . space
  . showsList showsConsTerm conss
  . showsString ")"

showsRhs :: Rhs -> ShowS
showsRhs (SimpleRhs pos expr decls)
  = showsString "(SimpleRhs "
  . showsPosition pos . space
  . showsExpression expr . space
  . showsList showsDecl decls
  . showsString ")"
showsRhs (GuardedRhs cexps decls)
  = showsString "(GuardedRhs "
  . showsList showsCondExpr cexps . space
  . showsList showsDecl decls
  . showsString ")"

showsCondExpr :: CondExpr -> ShowS
showsCondExpr (CondExpr pos exp1 exp2)
  = showsString "(CondExpr "
  . showsPosition pos . space
  . showsExpression exp1 . space
  . showsExpression exp2
  . showsString ")"

showsLiteral :: Literal -> ShowS
showsLiteral (Char _ c)
  = showsString "(Char "
  . shows c
  . showsString ")"
showsLiteral (Int ident n)
  = showsString "(Int "
  . showsIdent ident . space
  . shows n
  . showsString ")"
showsLiteral (Float _ x)
  = showsString "(Float "
  . shows x
  . showsString ")"
showsLiteral (String _ s)
  = showsString "(String "
  . shows s
  . showsString ")"

showsConsTerm :: Pattern -> ShowS
showsConsTerm (LiteralPattern lit)
  = showsString "(LiteralPattern "
  . showsLiteral lit
  . showsString ")"
showsConsTerm (NegativePattern ident lit)
  = showsString "(NegativePattern "
  . showsIdent ident . space
  . showsLiteral lit
  . showsString ")"
showsConsTerm (VariablePattern ident)
  = showsString "(VariablePattern "
  . showsIdent ident
  . showsString ")"
showsConsTerm (ConstructorPattern qident conss)
  = showsString "(ConstructorPattern "
  . showsQualIdent qident . space
  . showsList showsConsTerm conss
  . showsString ")"
showsConsTerm (InfixPattern cons1 qident cons2)
  = showsString "(InfixPattern "
  . showsConsTerm cons1 . space
  . showsQualIdent qident . space
  . showsConsTerm cons2
  . showsString ")"
showsConsTerm (ParenPattern cons)
  = showsString "(ParenPattern "
  . showsConsTerm cons
  . showsString ")"
showsConsTerm (TuplePattern _ conss)
  = showsString "(TuplePattern "
  . showsList showsConsTerm conss
  . showsString ")"
showsConsTerm (ListPattern _ conss)
  = showsString "(ListPattern "
  . showsList showsConsTerm conss
  . showsString ")"
showsConsTerm (AsPattern ident cons)
  = showsString "(AsPattern "
  . showsIdent ident . space
  . showsConsTerm cons
  . showsString ")"
showsConsTerm (LazyPattern _ cons)
  = showsString "(LazyPattern "
  . showsConsTerm cons
  . showsString ")"
showsConsTerm (FunctionPattern qident conss)
  = showsString "(FunctionPattern "
  . showsQualIdent qident . space
  . showsList showsConsTerm conss
  . showsString ")"
showsConsTerm (InfixFuncPattern cons1 qident cons2)
  = showsString "(InfixFuncPattern "
  . showsConsTerm cons1 . space
  . showsQualIdent qident . space
  . showsConsTerm cons2
  . showsString ")"
showsConsTerm (RecordPattern cfields mcons)
  = shows "(RecordPattern "
  . showsList (showsField showsConsTerm) cfields . space
  . showsMaybe showsConsTerm mcons
  . showsString ")"

showsExpression :: Expression -> ShowS
showsExpression (Literal lit)
  = showsString "(Literal "
  . showsLiteral lit
  . showsString ")"
showsExpression (Variable qident)
  = showsString "(Variable "
  . showsQualIdent qident
  . showsString ")"
showsExpression (Constructor qident)
  = showsString "(Constructor "
  . showsQualIdent qident
  . showsString ")"
showsExpression (Paren expr)
  = showsString "(Paren "
  . showsExpression expr
  . showsString ")"
showsExpression (Typed expr typ)
  = showsString "(Typed "
  . showsExpression expr . space
  . showsTypeExpr typ
  . showsString ")"
showsExpression (Tuple _ exps)
  = showsString "(Tuple "
  . showsList showsExpression exps
  . showsString ")"
showsExpression (List _ exps)
  = showsString "(List "
  . showsList showsExpression exps
  . showsString ")"
showsExpression (ListCompr _ expr stmts)
  = showsString "(ListCompr "
  . showsExpression expr . space
  . showsList showsStatement stmts
  . showsString ")"
showsExpression (EnumFrom expr)
  = showsString "(EnumFrom "
  . showsExpression expr
  . showsString ")"
showsExpression (EnumFromThen exp1 exp2)
  = showsString "(EnumFromThen "
  . showsExpression exp1 . space
  . showsExpression exp2
  . showsString ")"
showsExpression (EnumFromTo exp1 exp2)
  = showsString "(EnumFromTo "
  . showsExpression exp1 . space
  . showsExpression exp2
  . showsString ")"
showsExpression (EnumFromThenTo exp1 exp2 exp3)
  = showsString "(EnumFromThenTo "
  . showsExpression exp1 . space
  . showsExpression exp2 . space
  . showsExpression exp3
  . showsString ")"
showsExpression (UnaryMinus ident expr)
  = showsString "(UnaryMinus "
  . showsIdent ident . space
  . showsExpression expr
  . showsString ")"
showsExpression (Apply exp1 exp2)
  = showsString "(Apply "
  . showsExpression exp1 . space
  . showsExpression exp2
  . showsString ")"
showsExpression (InfixApply exp1 op exp2)
  = showsString "(InfixApply "
  . showsExpression exp1 . space
  . showsInfixOp op . space
  . showsExpression exp2
  . showsString ")"
showsExpression (LeftSection expr op)
  = showsString "(LeftSection "
  . showsExpression expr . space
  . showsInfixOp op
  . showsString ")"
showsExpression (RightSection op expr)
  = showsString "(RightSection "
  . showsInfixOp op . space
  . showsExpression expr
  . showsString ")"
showsExpression (Lambda _ conss expr)
  = showsString "(Lambda "
  . showsList showsConsTerm conss . space
  . showsExpression expr
  . showsString ")"
showsExpression (Let decls expr)
  = showsString "(Let "
  . showsList showsDecl decls . space
  . showsExpression expr
  . showsString ")"
showsExpression (Do stmts expr)
  = showsString "(Do "
  . showsList showsStatement stmts . space
  . showsExpression expr
  . showsString ")"
showsExpression (IfThenElse _ exp1 exp2 exp3)
  = showsString "(IfThenElse "
  . showsExpression exp1 . space
  . showsExpression exp2 . space
  . showsExpression exp3
  . showsString ")"
showsExpression (Case _ ct expr alts)
  = showsString "(Case "
  . showsCaseType ct . space
  . showsExpression expr . space
  . showsList showsAlt alts
  . showsString ")"
showsExpression (RecordConstr efields)
  = showsString "(RecordConstr "
  . showsList (showsField showsExpression) efields
  . showsString ")"
showsExpression (RecordSelection expr ident)
  = showsString "(RecordSelection "
  . showsExpression expr . space
  . showsIdent ident
  . showsString ")"
showsExpression (RecordUpdate efields expr)
  = showsString "(RecordUpdate "
  . showsList (showsField showsExpression) efields . space
  . showsExpression expr
  . showsString ")"

showsInfixOp :: InfixOp -> ShowS
showsInfixOp (InfixOp qident)
  = showsString "(InfixOp "
  . showsQualIdent qident
  . showsString ")"
showsInfixOp (InfixConstr qident)
  = showsString "(InfixConstr "
  . showsQualIdent qident
  . showsString ")"

showsStatement :: Statement -> ShowS
showsStatement (StmtExpr _ expr)
  = showsString "(StmtExpr "
  . showsExpression expr
  . showsString ")"
showsStatement (StmtDecl decls)
  = showsString "(StmtDecl "
  . showsList showsDecl decls
  . showsString ")"
showsStatement (StmtBind _ cons expr)
  = showsString "(StmtBind "
  . showsConsTerm cons . space
  . showsExpression expr
  . showsString ")"

showsCaseType :: CaseType -> ShowS
showsCaseType Rigid = showsString "Rigid"
showsCaseType Flex  = showsString "Flex"

showsAlt :: Alt -> ShowS
showsAlt (Alt pos cons rhs)
  = showsString "(Alt "
  . showsPosition pos . space
  . showsConsTerm cons . space
  . showsRhs rhs
  . showsString ")"

showsField :: (a -> ShowS) -> Field a -> ShowS
showsField sa (Field pos ident a)
  = showsString "(Field "
  . showsPosition pos . space
  . showsIdent ident . space
  . sa a
  . showsString ")"

showsPosition :: Position -> ShowS
showsPosition Position { line = l, column = c } = showsPair shows shows (l, c)
showsPosition _ = showsString "NoPos"
-- showsPosition (Position file row col)
--   = showsString "(Position "
--   . shows file . space
--   . shows row . space
--   . shows col
--   . showsString ")"

showsString :: String -> ShowS
showsString = (++)

space :: ShowS
space = showsString " "

newline :: ShowS
newline = showsString "\n"

showsMaybe :: (a -> ShowS) -> Maybe a -> ShowS
showsMaybe shs = maybe (showsString "Nothing")
                       (\x -> showsString "(Just " . shs x . showsString ")")

showsList :: (a -> ShowS) -> [a] -> ShowS
showsList _   [] = showsString "[]"
showsList shs (x:xs)
  = showsString "["
  . foldl (\sys y -> sys . showsString "," . shs y) (shs x) xs
  . showsString "]"

showsPair :: (a -> ShowS) -> (b -> ShowS) -> (a,b) -> ShowS
showsPair sa sb (a,b)
  = showsString "(" . sa a . showsString "," . sb b . showsString ")"

showsIdent :: Ident -> ShowS
showsIdent (Ident p x n)
  = showsString "(Ident " . showsPosition p . space
  . shows x . space . shows n . showsString ")"

showsQualIdent :: QualIdent -> ShowS
showsQualIdent (QualIdent mident ident)
  = showsString "(QualIdent "
  . showsMaybe showsModuleIdent mident
  . space
  . showsIdent ident
  . showsString ")"

showsModuleIdent :: ModuleIdent -> ShowS
showsModuleIdent = shows . moduleName
