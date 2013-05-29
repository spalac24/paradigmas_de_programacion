------------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing Curry programs
--- in Curry (type "CurryProg") and an I/O action to read Curry programs and
--- transform them into this abstract representation (function "readCurry").
---
--- Note this defines a slightly new format for AbstractCurry
--- in comparison to the first proposal of 2003.
---
--- Assumption: an abstract Curry program is stored in file with extension .acy
---
--- @author Michael Hanus
--- @version July 2008
------------------------------------------------------------------------------

module AbstractCurry where

------------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ==================================================================

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
--- <CODE>
---  (CProg modname imports typedecls functions opdecls)
--- </CODE>
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls, opdecls, functions: see below

data CurryProg = CurryProg String [String] [CTypeDecl] [CFuncDecl] [COpDecl]
  deriving (Read,Show)

--- The data type for representing qualified names.
--- In AbstractCurry all names are qualified to avoid name clashes.
--- The first component is the module name and the second component the
--- unqualified name as it occurs in the source program.
type QName = (String,String)


-- Data type to specify the visibility of various entities.

data CVisibility = Public    -- exported entity
                 | Private   -- private entity
  deriving (Read,Show)


--- The data type for representing type variables.
--- They are represented by (i,n) where i is a type variable index
--- which is unique inside a function and n is a name (if possible,
--- the name written in the source program).
type CTVarIName = (Int,String)

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
---
--- A data type definition of the form
---
--- <code>data t x1...xn = ...| c t1....tkc |...</code>
---
--- is represented by the Curry term
---
--- <code>(CType t v [i1,...,in] [...(CCons c kc v [t1,...,tkc])...])</code>
---
--- where each <code>ij</code> is the index of the type variable
--- <code> xj</code>.
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
---

data CTypeDecl = CType    QName CVisibility [CTVarIName] [CConsDecl]
               | CTypeSyn QName CVisibility [CTVarIName] CTypeExpr
  deriving (Read,Show)


--- A constructor declaration consists of the name and arity of the
--- constructor and a list of the argument types of the constructor.

data CConsDecl = CCons QName Int CVisibility [CTypeExpr]
  deriving (Read,Show)


--- Data type for type expressions.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO", "Success",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)

data CTypeExpr =
    CTVar CTVarIName               -- type variable
  | CFuncType CTypeExpr CTypeExpr  -- function type t1->t2
  | CTCons QName [CTypeExpr]       -- type constructor application
  deriving (Read,Show)
                                   -- (CTCons (module,name) arguments)


--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractCurry term (COp n fix p).

data COpDecl = COp QName CFixity Int
  deriving (Read,Show)

data CFixity = CInfixOp   -- non-associative infix operator
             | CInfixlOp  -- left-associative infix operator
             | CInfixrOp  -- right-associative infix operator
  deriving (Read,Show)


--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)
--- where i is a variable index.

type CVarIName = (Int,String)


--- Data type for representing function declarations.
--- 
--- A function declaration in AbstractCurry is a term of the form
---
--- <code>(CFunc name arity visibility type (CRules eval [CRule rule1,...,rulek]))</code>
---
--- and represents the function <code>name</code> defined by the rules
--- <code>rule1,...,rulek</code>.
---
--- Note: the variable indices are unique inside each rule
---
--- External functions are represented as
--- <code>(CFunc name arity type (CExternal s))</code>
--- where s is the external name associated to this function.
---
--- Thus, a function declaration consists of the name, arity, type, and
--- a list of rules.
---
--- A function declaration with the constructor <code>CmtFunc</code>
--- is similarly to <code>CFunc</code> but has a comment
--- as an additional first argument. This comment could be used
--- by pretty printers that generate a readable Curry program
--- containing documentation comments.

data CFuncDecl = CFunc QName Int CVisibility CTypeExpr CRules
               | CmtFunc String QName Int CVisibility CTypeExpr CRules
  deriving (Read,Show)


--- A rule is either a list of formal parameters together with an expression
--- (i.e., a rule in flat form), a list of general program rules with
--- an evaluation annotation, or it is externally defined

data CRules = CRules CEvalAnnot [CRule]
            | CExternal String
  deriving (Read,Show)

--- Data type for classifying evaluation annotations for functions.
--- They can be either flexible (default), rigid, or choice.

data CEvalAnnot = CFlex | CRigid | CChoice
  deriving (Read,Show)

--- The most general form of a rule. It consists of a list of patterns
--- (left-hand side), a list of guards ("success" if not present in the
--- source text) with their corresponding right-hand sides, and
--- a list of local declarations.
data CRule = CRule [CPattern] [(CExpr,CExpr)] [CLocalDecl]
  deriving (Read,Show)

--- Data type for representing local (let/where) declarations
data CLocalDecl =
     CLocalFunc CFuncDecl                   -- local function declaration
   | CLocalPat  CPattern CExpr [CLocalDecl] -- local pattern declaration
   | CLocalVar  CVarIName                   -- local free variable declaration
  deriving (Read,Show)

--- Data type for representing Curry expressions.

data CExpr =
   CVar      CVarIName              -- variable (unique index / name)
 | CLit      CLiteral               -- literal (Integer/Float/Char constant)
 | CSymbol   QName                  -- a defined symbol with module and name
 | CApply    CExpr CExpr            -- application (e1 e2)
 | CLambda   [CPattern] CExpr       -- lambda abstraction
 | CLetDecl  [CLocalDecl] CExpr     -- local let declarations
 | CDoExpr   [CStatement]           -- do expression
 | CListComp CExpr [CStatement]     -- list comprehension
 | CCase     CExpr [CBranchExpr]    -- case expression
  deriving (Read,Show)

--- Data type for representing statements in do expressions and
--- list comprehensions.

data CStatement = CSExpr CExpr         -- an expression (I/O action or boolean)
                | CSPat CPattern CExpr -- a pattern definition
                | CSLet [CLocalDecl]   -- a local let declaration
  deriving (Read,Show)

--- Data type for representing pattern expressions.

data CPattern =
   CPVar CVarIName               -- pattern variable (unique index / name)
 | CPLit CLiteral                -- literal (Integer/Float/Char constant)
 | CPComb QName [CPattern]       -- application (m.c e1 ... en) of n-ary
                                 -- constructor m.c (CPComb (m,c) [e1,...,en])
 | CPAs       CVarIName CPattern -- as-pattern (extended Curry)
 | CPFuncComb QName [CPattern]   -- function pattern (extended Curry)
  deriving (Read,Show)

--- Data type for representing branches in case expressions.

data CBranchExpr = CBranch CPattern CExpr
  deriving (Read,Show)

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, or a character constant.

data CLiteral = CIntc   Integer
              | CFloatc Rational
              | CCharc  Char
  deriving (Read,Show)

