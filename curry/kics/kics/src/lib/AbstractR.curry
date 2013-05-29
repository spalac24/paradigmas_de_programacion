------------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing Curry programs
--- in Curry (type "CurryProg") and an I/O action to read Curry programs and
--- transform them into this abstract representation (function "readCurry").
---
--- Note: this module defines an extended format of AbstractCurry in
--- comparison to the version from april 2004 using labeled fields (records).
---
--- Assumption: an abstract Curry program is stored in file "prog.abstract".
---
--- This modules should be imported qualified to avoid name clashes with
--- other modules (e.g. with "FlatCurry"). It might be comfortable to
--- use the following import declaration:
---
---      import qualified AbstractR as C
---
--- This allows the usage of qualifier "C" (like "C.CurryProg" or 
--  "C.TypeDecl")
--- instead of using the longer qualifier "AbstractR".
---
--- @author Michael Hanus, Bernd Brassel, Martin Engelke
--- @version July 2006
------------------------------------------------------------------------------

module AbstractR where   

------------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ==================================================================

--- Record for representing a Curry module in the intermediate form.
--- Label description:
---    moduleName - name of this module
---    imports    - list of module names that are imported
---    exports    - list of module names that are fully exported
---    typeDecls  - list of type declarations (see below)
---    funcDecls  - list of function declarations (see below)
---    opDecls    - list of operator delcarations (see below)
type CurryProg = { moduleName :: String,
                   imports,
                   exports    :: [String],
                   typeDecls  :: [TypeDecl],
                   funcDecls  :: [FuncDecl],
                   opDecls    :: [OpDecl]
                 }


--- Record for representing qualified names.
--- In AbstractCurry all names imported from different modules 
--- are qualified to avoid name clashes.
--- for imported qualifiers, the first string is the module name 
--- and the second is the unqualified identifier as it occurs 
--- in the source program.
data QName = This  String
           | Other String String

{- alternative?
type QName = { qualifier :: String,
               name      :: String
             }
-}

--- Type for representing identifiers for variable of any kind
--- (if possible, the identifier written in the source program).
type VarName = String


-- Type for representing identifiers for labels (extended Curry).
type Label = String


-- Data type to specify the visibility of various entities.
data Visibility = Public    -- exported entity
                | Private   -- private entity


--- Record for representing definitions of algebraic data types
--- and type synonyms.
--- Label description:
---    typeName         - name of the type
---    typeVisibility - type visibility
---    typeParameters - list type parameters (type variables)
---    typeMode       - rhs of the data type or the type synonym
type TypeDecl = { typeName       :: QName,
                  typeVisibility :: Visibility,
                  typeParameters :: [VarName],
                  typeDef        :: Either DataDeclDescr TypeSynDescr
                }

--- oder besser: typeMode       ::TypeMode ???
--- Data type for representing the right-hand-side of algebraic data types
--- (= list of constructor declarations) or type synonyms (= type expression).
data TypeMode = DataDecl DataDeclDescr  -- rhs of a "data" declaration
              | TypeSyn TypeSynDescr    -- rhs of a "type" declaration

type DataDeclDescr = {constructors :: [ConsDecl]}
type TypeSynDescr = {typeExpr :: TypeExpr}


--- Record for representing a constructor declaration.
--- Label description:
---    consName         - name of the constructor
---    consVisibility - constructor visibility
---    consArguments  - list of argument types of the constructor
type ConsDecl = { consName         :: QName,
                  consVisibility :: Visibility,
                  consArguments  :: [TypeExpr]
                }


--- Data type for representing type expressions.
--- A type expression is either a type variable, a function type,
--- a type constructor application or a record type.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO", "Success",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)
data TypeExpr =
    TVar TVarDescr             -- type variable
  | FuncType FuncTypeDescr     -- function type t1->t2
  | TCons TConsDescr           -- type constructor application
  | RecordType RecordTypeDescr -- record type (extended Curry)

type TVarDescr = {typeVarName :: VarName}
type FuncTypeDescr = {fromType :: TypeExpr, toType :: TypeExpr}
type TConsDescr = {typeConsName :: QName, typeConsArgs :: [TypeExpr]}
type RecordTypeDescr = {typeFields :: [Field TypeExpr],
                        baseRecord :: Maybe VarName}


--- Record for representing operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractCurry term {opName=n, opFixity=fix, opPrecedence=p}.
type OpDecl = { opName         :: QName,
                opFixity     :: Fixity,
                opPrecedence :: Int
              }


--- Data type for representing several kinds of fixities for operators
data Fixity = InfixOp   -- non-associative infix operator
            | InfixlOp  -- left-associative infix operator
            | InfixrOp  -- right-associative infix operator


--- Record for representing function declarations.
--- A function
---
---   fname :: type
---   rule_1
---     :
---   rule_k
---
--- in Curry is represented by a record of the form
---
---    { funcName         = fname,
---      funcVisibility = vis,
---      funcType       = Just type,
---      funcRules      = rules
---    }
---
--- where 'vis' is the visibility of the function (defined in the export
--- list of the Curry program) and 'rules' is a term of the type 'Rules'
--- (see below) containing the list of function rules [rule_1, ..., rule_k].
---
--- Label description:
---    funcName         - name of the function
---    funcVisibility - function visibility
---    funcType       - type (annotation) of the function
---    funcRules      - function rules (Just) or 'external' annotation (Nothing)
---
--- Note: since it is not necessary to explicitly annotated the function type
---       in the program, the type information is embedded into a 'Maybe' term
type FuncDecl = { funcName         :: QName,
                  funcVisibility :: Visibility,
                  funcType       :: Maybe TypeExpr,
                  funcRule       :: [Rule]
                }


--- Record for representing the most general form of a function rule.
--- Label description:
---    patterns   - list of patterns from the rule lhs
---    rhs        - rule rhs
---    localDecls - list of local ("where") declarations
type Rule = { patterns   :: [Pattern],
              rhs        :: Rhs,
              localDecls :: [LocalDecl]
            }

--- Record for representing right-hand-sides (e.g. in function rules).
--- A right-hand-side can either be a simple expression or a list of guarded
--- exprssions (see below)·
--- Note: a simple expression is (semantically) equivalent to the corresponding
---       guarded expression with the guard "success".
type Rhs = Either SimpleExprDescr GuardedExprsDescr

--- oder besser?
--data Rhs = SimpleExpr SimpleExprDescr
--         | GuardedExprs GuardedExprsDescr

type SimpleExprDescr = {simpleExpr :: Expr}
type GuardedExprsDescr = {guardedExprs :: [GuardedExpr]}


--- Record for representing guarded expressions.
--- A guarded expression consists of a guard and a simple expression.
--- A guard is an expression of type 'Bool' or 'Success'.
type GuardedExpr = { guard :: Expr,
                     expr  :: Expr
                   }




--- Data type for representing local ("let"/"where") declarations
data LocalDecl =
     LocalFunc LocalFuncDescr    -- local function declaration
   | LocalPat  LocalPatDescr     -- local pattern declaration
   | LocalVar LocalVarDescr      -- local free variable declaration

type LocalFuncDescr = {localFunction :: FuncDecl}
type LocalPatDescr = {localPattern    :: Pattern, 
                      localRhs        :: Rhs, 
                      localLocalDecls :: [LocalDecl]}
type LocalVarDescr = {localVarName :: VarName}


--- Data type for representing Curry expressions.
data Expr =
   Var       VarDescr         -- variable (name)
 | Lit       LitDescr         -- literal (Integer/Float/Char constant)
 | Symbol    SymbolDescr      -- a defined symbol with module and name
 | Apply     ApplyDescr       -- application (e1 e2)
 | Lambda    LambdaDescr      -- lambda abstraction
 | LetDecl   LetDeclDescr     -- local let declarations
 | DoExpr    DoExprDescr      -- do expression
 | ListComp  ListCompDescr    -- list comprehension
 | Case      CaseDescr        -- case expression
 | RecConstr RecConstrDescr   -- record construction (extended Curry)
 | RecSelect RecSelectDescr   -- field selection (extended Curry)
 | RecUpdate RecUpdateDescr   -- record update (extended Curry)

type VarDescr = {varName :: VarName}
type LitDescr = {literal :: Literal}
type SymbolDescr = {symbol :: QName}
type ApplyDescr = {function :: Expr, argument :: Expr}
type LambdaDescr = {lambdaLhs :: [Pattern], lambdaRhs :: Expr}
type LetDeclDescr = {letDecls :: [LocalDecl], letExpr :: Expr}
type DoExprDescr = {doStatements :: [Statement]}
type ListCompDescr = {listCompExpr :: Expr, listCompStatements :: [Statement]}
type CaseDescr = {caseExpr :: Expr, branches :: [BranchExpr]}
type RecConstrDescr = {fieldConstr :: [Field Expr]}
type RecSelectDescr = {recordExpr :: Expr, fieldSelection :: Label}
type RecUpdateDescr = {fieldUpdate :: [Field Expr], recordUpdate :: Expr}

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, or a character constant.
data Literal = Intc   IntcDescr
             | Floatc FloatcDescr
             | Charc  CharcDescr

type IntcDescr = {intValue :: Int}
type FloatcDescr = {floatValue :: Float}
type CharcDescr = {charValue :: Char}

--- Data type for representing statements in do expressions and
--- list comprehensions.
data Statement = SExpr SExprDescr  -- an expression (I/O action or boolean)
               | SPat SPatDescr    -- a pattern definition
               | SLet SLetDescr    -- a local let declaration

type SExprDescr = {stmtExpr :: Expr}
type SPatDescr = {stmtPattern :: Pattern, stmtPatternRhs :: Expr}
type SLetDescr = {stmtLetDecls :: [LocalDecl]}


--- Data type for representing pattern terms.
data Pattern =
   PVar PVarDescr           -- pattern variable (name)
 | PLit PLitDescr           -- literal (Integer/Float/Char constant)
 | PComb PCombDescr         -- application (m.c e1 ... en) of n-ary
                            -- constructor m.c (PComb (m,c) [e1,...,en])
 | PAs PAsDescr             -- as-pattern (extended Curry)
 | PFuncComb PFuncCombDescr -- function pattern (extended Curry)
 | PLazy PLazyDescr         -- lazy pattern (extended Curry)
 | PRecord PRecordDescr     -- record pattern (extended Curry)

type PVarDescr = {pattVarName :: VarName}
type PLitDescr = {pattLiteral :: Literal}
type PCombDescr = {pattConsName :: QName, pattArgs :: [Pattern]}
type PAsDescr = {pattAlias :: VarName, pattPattern :: Pattern}
type PFuncCombDescr = {pattFuncName :: QName, pattFuncArgs :: [Pattern]}
type PLazyDescr = {pattLazy :: Pattern}
type PRecordDescr = {pattFields :: [Field Pattern],
                     pattRecord :: Maybe Pattern}


--- Record for representing branches in case expressions.
type BranchExpr = { branchPattern :: Pattern,
                    branchRhs     :: Rhs
                  }


--- Record for representing record fields.
type Field a = { fieldLabel   :: Label,
                 fieldContent :: a
               }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
