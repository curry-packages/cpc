-- ---------------------------------------------------------------------------
--- This library contains a definition for representing Curry programs
--- in Curry and an I/O action to read Curry programs and
--- transform them into this abstract representation.
---
--- Note this defines a slightly new format for AbstractCurry
--- in comparison to the first proposal of 2003.
---
--- Assumption: an abstract Curry program is stored in file with
--- extension .acy
---
--- @author Michael Hanus, Bjoern Peemoeller, Jan Tikovsky
--- @version September 2016
--- @category meta
-- ---------------------------------------------------------------------------

module ACSpans.AbstractCurrySpan where

import ACSpans.Span


-- ---------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ---------------------------------------------------------------------------

--- Current version of AbstractCurry
version :: String
version = "AbstractCurry 1.0"

--- A module name.
type MName = String

--- The data type for representing qualified names.
--- In AbstractCurry all names are qualified to avoid name clashes.
--- The first component is the module name and the second component the
--- unqualified name as it occurs in the source program.
--- An exception are locally defined names where the module name is
--- the empty string (to avoid name clashes with a globally defined name).
type QName = (Span, MName, String)

--- Data type to specify the visibility of various entities.
data CVisibility
  = Public    -- exported entity
  | Private   -- private entity
  
--- Data type for operator associativity
data CFixity
  = CInfixOp   -- non-associative infix operator
  | CInfixlOp  -- left-associative infix operator
  | CInfixrOp  -- right-associative infix operator
  
--- Type of case expressions
data CCaseType
  = CRigid -- rigid case expression
  | CFlex  -- flexible case expression


--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
---
---     (CurryProg modname imports typedecls functions opdecls)
---
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls: Type declarations
---       functions: Function declarations
---       opdecls: Operator precedence declarations
data CurryProg = CurryProg Span MName [MName] [CTypeDecl] [CFuncDecl] [COpDecl]

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
---
--- A data type definition of the form
---
---     data t x1...xn = ...| c t1....tkc |...
---
--- is represented by the Curry term
---
---     (CType t v [i1,...,in] [...(CCons c kc v [t1,...,tkc])...])
---
--- where each `ij` is the index of the type variable `xj`.
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
data CTypeDecl
  = CType    Span QName CVisibility [CTVarIName] [CConsDecl]
  | CTypeSyn Span QName CVisibility [CTVarIName] CTypeExpr
  | CNewType Span QName CVisibility [CTVarIName] CConsDecl

--- The type for representing type variables.
--- They are represented by (i,n) where i is a type variable index
--- which is unique inside a function and n is a name (if possible,
--- the name written in the source program).
type CTVarIName = (Span, Int, String)

--- A constructor declaration consists of the name of the
--- constructor and a list of the argument types of the constructor.
--- The arity equals the number of types.
data CConsDecl
  = CCons   Span QName CVisibility [CTypeExpr]
  | CRecord Span QName CVisibility [CFieldDecl]

--- A record field declaration consists of the name of the
--- the label, the visibility and its corresponding type.
data CFieldDecl = CField Span QName CVisibility CTypeExpr

--- Type expression.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)
data CTypeExpr
  = CTVar     Span CTVarIName           -- type variable
  | CFuncType Span CTypeExpr CTypeExpr  -- function type t1->t2
  | CTCons    Span QName [CTypeExpr]    -- type constructor application
                                        -- (CTCons (module,name) arguments)

--- Labeled record fields
type CField a = (QName, a)

--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractCurry term (COp n fix p).
data COpDecl = COp Span QName CFixity Int

--- Data type for operator associativity
-- data CFixity
--   = CInfixOp   -- non-associative infix operator
--   | CInfixlOp  -- left-associative infix operator
--   | CInfixrOp  -- right-associative infix operator

--- Function arity
type Arity = Int

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
--- Thus, a function declaration consists of the name, arity, type, and
--- a list of rules.
---
--- A function declaration with the constructor <code>CmtFunc</code>
--- is similarly to <code>CFunc</code> but has a comment
--- as an additional first argument. This comment could be used
--- by pretty printers that generate a readable Curry program
--- containing documentation comments.
data CFuncDecl
  = CFunc   (Maybe Span)        QName Arity CVisibility CTypeExpr [CRule]
  | CmtFunc (Maybe Span) String QName Arity CVisibility CTypeExpr [CRule]

--- The general form of a function rule. It consists of a list of patterns
--- (left-hand side) and the right-hand side for these patterns.
data CRule = CRule Span [CPattern] CRhs

--- Right-hand-side of a 'CRule' or a `case` expression.
--- It is either a simple unconditional right-hand side or
--- a list of guards with their corresponding right-hand sides, and
--- a list of local declarations.
data CRhs
  = CSimpleRhs  Span CExpr                  [CLocalDecl] -- expr where decls
  | CGuardedRhs Span [(Span, CExpr, CExpr)] [CLocalDecl] -- | cond = expr where decls

--- Data type for representing local (let/where) declarations
data CLocalDecl
  = CLocalFunc Span CFuncDecl     -- local function declaration
  | CLocalPat  Span CPattern CRhs -- local pattern declaration
  | CLocalVars Span [CVarIName]   -- local free variable declaration

--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)--- where i is a variable index.
type CVarIName = (Span, Int, String)

--- Data type for representing pattern expressions.
data CPattern
  = CPVar      Span CVarIName               -- pattern variable (unique index / name)
  | CPLit      Span CLiteral                -- literal (Integer/Float/Char constant)
  | CPComb     Span QName [CPattern]        -- application (m.c e1 ... en) of n-ary
                                       -- constructor m.c (CPComb (m,c) [e1,...,en])
  | CPAs       Span CVarIName CPattern      -- as-pattern (extended Curry)
  | CPFuncComb Span QName [CPattern]        -- function pattern (extended Curry)
  | CPLazy     Span CPattern                -- lazy pattern (extended Curry)
  | CPRecord   Span QName [CField CPattern] -- record pattern (extended Curry)

--- Data type for representing Curry expressions.
data CExpr
 = CVar       Span CVarIName                          -- variable (unique index / name)
 | CLit       Span CLiteral                           -- literal (Integer/Float/Char constant)
 | CSymbol    Span QName                              -- a defined symbol with module and name
 | CApply     Span CExpr CExpr                        -- application (e1 e2)
 | CLambda    Span [CPattern] CExpr                   -- lambda abstraction
 | CLetDecl   Span [CLocalDecl] CExpr                 -- local let declarations
 | CDoExpr    Span [CStatement]                       -- do expression
 | CListComp  Span CExpr [CStatement]                 -- list comprehension
 | CCase      Span CCaseType CExpr [(CPattern, CRhs)] -- case expression
 | CTyped     Span CExpr CTypeExpr                    -- typed expression
 | CRecConstr Span QName [CField CExpr]               -- record construction (extended Curry)
 | CRecUpdate Span CExpr [CField CExpr]               -- record update (extended Curry)

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, or a character constant.
data CLiteral
  = CIntc    Span Int
  | CFloatc  Span Float
  | CCharc   Span Char
  | CStringc Span String

--- Data type for representing statements in do expressions and
--- list comprehensions.
data CStatement
  = CSExpr Span CExpr         -- an expression (I/O action or boolean)
  | CSPat  Span CPattern CExpr -- a pattern definition
  | CSLet  Span [CLocalDecl]   -- a local let declaration

--- Type of case expressions
-- data CCaseType
--   = CRigid -- rigid case expression
--   | CFlex  -- flexible case expression

---------------------------------------------------------------------------
--- The name of the standard prelude.
preludeName :: String
preludeName = "Prelude"

--- Converts a string into a qualified name of the Prelude.
pre :: String -> QName
pre f = (virtualSpan, preludeName, f)

---------------------------------------------------------------------------
