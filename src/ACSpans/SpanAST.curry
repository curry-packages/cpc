{- |
    Module      :  ACSpans.SpanAST
    Description :  Extension of Abstract Syntax Tree

    This module contains the description of the AST which has been
    extended by span information (i.e. start and end positions) (SpanAST).
    The inline comments document for which tokens span information was added.
-}
module ACSpans.SpanAST where

import ACSpans.AST   (KnownExtension (..), Tool (..))
import ACSpans.Ident (ModuleIdent, Ident, QualIdent, SymIdent, SymQualIdent)
import ACSpans.Span  (Span)

-- ---------------------------------------------------------------------------
-- Modules
-- ---------------------------------------------------------------------------

-- |Curry module
--  1. (Maybe Span) -  KW_module
--  2. (Maybe Span) -  KW_where
data Module = Module [ModulePragma] (Maybe Span) ModuleIdent (Maybe Span)
                     (Maybe ExportSpec) [ImportDecl] [Decl]

-- |Module pragma
data ModulePragma
  -- 1. Span   -  PragmaLanguage
  -- 2. [Span] -  Commas
  -- 3. Span   -  PragmaEnd
  = LanguagePragma Span [Extension] [Span] Span
  -- 1. Span   - PragmaOptions
  -- 2. Span   - PragmaEnd
  | OptionsPragma  Span (Maybe String) String Span

-- |Specified language extensions, either known or unknown.
data Extension
  = KnownExtension   Span KnownExtension   -- 1. Span  -  Id "KnownExtension"
  | UnknownExtension Span String           -- 1. Span  -  Id "Name"

-- |Export specification
-- 1. Span   -  LeftParen
-- 2. [Span] -  Commas
-- 3. Span   -  RightParen
data ExportSpec = Exporting Span [Export] [Span] Span

-- |Single exported entity
data Export
  = Export         SymQualIdent
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | ExportTypeWith QualIdent Span [Ident] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightParen
  | ExportTypeAll  QualIdent Span Span Span
  -- 1. Span   -  KW_module
  | ExportModule   Span ModuleIdent

-- |Import declaration
-- 1. Span         -  KW_import
-- 2. (Maybe Span) -  Id_qualified
-- 3. (Maybe Span) -  Id_as
data ImportDecl = ImportDecl Span (Maybe Span) ModuleIdent (Maybe Span) Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)

-- |Flag to signal qualified import
type Qualified = Bool

-- |Import specification
data ImportSpec
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  = Importing Span [Import] [Span] Span
  -- 1. Span   -  Id_hiding
  -- 2. Span   -  LeftParen
  -- 3. [Span] -  Commas
  -- 4. Span   -  RightParen
  | Hiding    Span Span [Import] [Span] Span

-- |Single imported entity
data Import
  = Import         SymIdent
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | ImportTypeWith Ident Span [Ident] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightParen
  | ImportTypeAll  Ident Span Span Span

-- ---------------------------------------------------------------------------
-- Declarations (local or top-level)
-- ---------------------------------------------------------------------------

-- |Declaration in a module
data Decl
  -- 1. [Span] -  Backquotes
  = InfixDecl    Infix (Maybe Precedence) [SymIdent] [Span]
  -- 1. Span         -  KW_data
  -- 2. (Maybe Span) -  Equals
  -- 3. [Span]       -  Bars
  | DataDecl     Span Ident [Ident] (Maybe Span) [ConstrDecl] [Span]
  -- 1. Span   -  KW_newtype
  -- 2. Span   -  Equals
  | NewtypeDecl  Span Ident [Ident] Span NewConstrDecl
  -- 1. Span   -  KW_type
  -- 2. Span   -  Equals
  | TypeDecl     Span Ident [Ident] Span TypeExpr
  -- 1. [Span] - Commas
  -- 2. Span   - DotDot
  | TypeSig      [SymIdent] [Span] Span TypeExpr
  | FunctionDecl Ident [Equation]
  -- 1. Span                   -  KW_foreign
  -- 2. (Maybe (Span, String)) -  Id "name.h"
  -- 3. Span                   -  DotDot
  | ForeignDecl  Span CallConv (Maybe (Span, String)) SymIdent Span TypeExpr
  -- 1. [Span] -  Commas
  -- 2. Span   -  KW_external
  | ExternalDecl [SymIdent] [Span] Span
  | PatternDecl  Pattern Rhs
  -- 1. [Span] -  Commas
  -- 2. Span   -  KW_free
  | FreeDecl     [Ident] [Span] Span

-- |Operator precedence
type Precedence = (Span, Int)

-- |Fixity of operators
data Infix
  = InfixL Span   -- 1. Span -  KW_InfixL
  | InfixR Span   -- 1. Span -  KW_InfixR
  | Infix  Span   -- 1. Span -  KW_Infix

-- |Constructor declaration for algebraic data types
data ConstrDecl
  = ConstrDecl [Ident] Ident [TypeExpr]
  | ConOpDecl  [Ident] TypeExpr Ident TypeExpr
  -- 1. Span   -  LeftBrace
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBrace
  | RecordDecl [Ident] Ident Span [FieldDecl] [Span] Span

-- |Constructor declaration for renaming types (newtypes)
data NewConstrDecl
  = NewConstrDecl [Ident] Ident TypeExpr
  -- 1. Span -  LeftBrace
  -- 2. Span -  DotDot
  -- 3. Span -  RightBrace
  | NewRecordDecl [Ident] Ident Span (Ident, Span, TypeExpr) Span

-- |Declaration for labelled fields
-- 1. [Span] -  Commas
-- 2. Span   -  DotDot
data FieldDecl = FieldDecl [Ident] [Span] Span TypeExpr

-- |Calling convention for C code
data CallConv
  = CallConvPrimitive Span    -- 1. Span -  Id_primitive
  | CallConvCCall     Span    -- 1. Span -  Id_ccall

-- |Type expressions
data TypeExpr
  -- 1. (Maybe Span) - LeftParen
  -- 2. (Maybe Span) - RightParen
  = ConstructorType (Maybe Span) QualIdent [TypeExpr] (Maybe Span)
  | VariableType    Ident
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | TupleType       Span [TypeExpr] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  RightBracket
  | ListType        Span TypeExpr Span
  -- 1. Span   - RightArrow
  | ArrowType       TypeExpr Span TypeExpr
  -- 1. Span   - LeftParen
  -- 2. Span   - RightParen
  | ParenType       Span TypeExpr Span

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- |Function defining equation
data Equation = Equation Lhs Rhs

-- |Left-hand-side of an 'Equation' (function identifier and patterns)
data Lhs
  = FunLhs SymIdent [Pattern]
  | OpLhs  Pattern SymIdent Pattern
  | ApLhs  Lhs [Pattern]

-- |Right-hand-side of an 'Equation'
data Rhs
  -- 1. Span         -  Equals or RightArrow
  -- 2. (Maybe Span) -  KW_where
  = SimpleRhs  Span Expression (Maybe Span) [Decl]
  -- 1. Span         -  First Bar
  -- 2. [Span]       -  more Bars
  -- 3. (Maybe Span) -  KW_where
  | GuardedRhs Span [CondExpr] [Span] (Maybe Span) [Decl]

-- |Conditional expression (expression conditioned by a guard)
data CondExpr = CondExpr Expression Span Expression  -- 1. Span -  Equals

-- |Literal
data Literal
  = Char   Span Char       -- 1. Span -  CharTok
  | Int    Span Int        -- 1. Span -  IntTok
  | Float  Span Float      -- 1. Span -  FloatTok
  | String Span String     -- 1. Span -  StringTok

-- |Constructor term (used for patterns)
data Pattern
  = LiteralPattern     Literal
  | NegativePattern    Ident Literal
  | VariablePattern    Ident
  | ConstructorPattern QualIdent [Pattern]
  | InfixPattern       Pattern QualIdent Pattern
  -- 1. Span   -  LeftPattern
  -- 2. Span   -  RightPattern
  | ParenPattern       Span Pattern Span
  -- 1. Span   -  LeftBrace
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBrace
  | RecordPattern      QualIdent Span [Field Pattern] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | TuplePattern       Span [Pattern] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBracket
  | ListPattern        Span [Pattern] [Span] Span
  -- 1. Span   -  At
  | AsPattern          Ident Span Pattern
  -- 1. Span   -  Tilde
  | LazyPattern        Span Pattern
  | FunctionPattern    QualIdent [Pattern]
  | InfixFuncPattern   Pattern QualIdent Pattern

-- |Expression
data Expression
  = Literal           Literal
  | Variable          SymQualIdent
  | Constructor       SymQualIdent
  -- 1. Span   -  LeftParen
  -- 2. Span   -  RightParen
  | Paren             Span Expression Span
  -- 1. Span   -  DotDot
  | Typed             Expression Span TypeExpr
  -- 1. Span   -  LeftBrace
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBrace
  | Record            QualIdent Span [Field Expression] [Span] Span
  -- 1. Span   -  LeftBrace
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBrace
  | RecordUpdate      Expression Span [Field Expression] [Span] Span
  -- 1. Span   -  LeftParen
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightParen
  | Tuple             Span [Expression] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. [Span] -  Commas
  -- 3. Span   -  RightBracket
  | List              Span [Expression] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  Bar
  -- 3. [Span] -  Commas
  -- 4. Span   -  RightBracket
  | ListCompr         Span Expression Span [Statement] [Span] Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightBracket
  | EnumFrom          Span Expression Span Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  Comma
  -- 3. Span   -  DotDot
  -- 4. Span   -  RightBracket
  | EnumFromThen      Span Expression Span Expression Span Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  DotDot
  -- 3. Span   -  RightBracket
  | EnumFromTo        Span Expression Span Expression Span
  -- 1. Span   -  LeftBracket
  -- 2. Span   -  Comma
  -- 3. Span   -  DotDot
  -- 4. Span   -  RightBracket
  | EnumFromThenTo    Span Expression Span Expression Span Expression Span
  | UnaryMinus        Ident Expression
  | Apply             Expression Expression
  | InfixApply        Expression InfixOp Expression
  -- 1. Span   - LeftParen
  -- 2. Span   - RightParen
  | LeftSection       Span Expression InfixOp Span
  -- 1. Span   - LeftParen
  -- 2. Span   - RightParen
  | RightSection      Span InfixOp Expression Span
  -- 1. Span   - Backslash
  -- 2. Span   - RightArrow
  | Lambda            Span [Pattern] Span Expression
  -- 1. Span   - KW_let
  -- 2. Span   - KW_in
  | Let               Span [Decl] Span Expression
  -- 1. Span   - KW_do
  | Do                Span [Statement] Expression
  -- 1. Span   - KW_if
  -- 2. Span   - KW_then
  -- 3. Span   - KW_else
  | IfThenElse        Span Expression Span Expression Span Expression
  -- 1. Span   - KW_case
  -- 2. Span   - KW_of
  | Case              CaseType Span Expression Span [Alt]

-- |Infix operation
data InfixOp
  = InfixOp     SymQualIdent
  | InfixConstr SymQualIdent

-- |Statement
data Statement
  = StmtExpr Expression
  | StmtDecl Span [Decl]                -- 1. Span -  KW_let
  | StmtBind Span Pattern Expression    -- 1. Span -  LeftArrow

-- |Type of case expressions
data CaseType
  = Rigid
  | Flex

-- |Single case alternative
data Alt = Alt Pattern Rhs

-- |Record field
data Field a = Field QualIdent Span a    -- 1. Span -  Equals
