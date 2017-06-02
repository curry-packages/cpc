{- |
    Module      :  ACSpans.AST
    Description :  Abstract Syntax Tree

    This module contains the description of the curry abstract syntax tree (AST)
    and useful functions on the elements of the AST.
-}
module ACSpans.AST where

import Char (isAlphaNum)
import List (intercalate)

import ACSpans.Span (Pos)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- |Simple identifier
data Ident = Ident
  { idPosition :: Pos      -- ^ Source code 'Position'
  , idName     :: String   -- ^ Name of the identifier
  , idUnique   :: Int      -- ^ Unique number of the identifier
  }

-- |Qualified identifier
data QualIdent = QualIdent
  { qidModule :: Maybe ModuleIdent -- ^ optional module identifier
  , qidIdent  :: Ident             -- ^ identifier itself
  }

-- | Module identifier
data ModuleIdent = ModuleIdent
  { midPosition   :: Pos      -- ^ source code 'Position'
  , midQualifiers :: [String] -- ^ hierarchical idenfiers
  }

-- |Specified language extensions, either known or unknown.
data Extension
  = KnownExtension   Pos KnownExtension -- ^ a known extension
  | UnknownExtension Pos String         -- ^ an unknown extension

data KnownExtension
  = AnonFreeVars       -- ^ anonymous free variables
  | FunctionalPatterns -- ^ functional patterns
  | NegativeLiterals   -- ^ negative literals
  | NoImplicitPrelude  -- ^ no implicit import of the prelude

-- |Different Curry tools which may accept compiler options.
data Tool = KICS2 | PAKCS | CYMAKE | UnknownTool String


-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- |Hierarchical module name
moduleName :: ModuleIdent -> String
moduleName = intercalate "." . midQualifiers

-- |Hierarchical name of qualified Ident
qidName :: QualIdent -> String
qidName q = case q of
  (QualIdent Nothing  i) -> idName i
  (QualIdent (Just m) i) -> intercalate "." [(moduleName m), (idName i)]

-- |Set position of ModuleIdent
setMIdPos :: Pos -> ModuleIdent -> ModuleIdent
setMIdPos p m = m { midPosition = p }

-- |Set position of Ident
setIdPos :: Pos -> Ident -> Ident
setIdPos p i = i { idPosition = p }

-- |Set position of QualIdent
setQIdPos :: Pos -> QualIdent -> QualIdent
setQIdPos p q = q { qidIdent = setIdPos p (qidIdent q) }

-- |Check whether an 'Ident' identifies an infix operation
isInfixOp :: Ident -> Bool
isInfixOp (Ident _ s _) = all (`elem` "~!@#$%^&*+-=<>:?./|\\") s


-- ---------------------------------------------------------------------------
-- Definition of AST: Module
-- ---------------------------------------------------------------------------

-- |Curry module
data Module = Module [ModulePragma] ModuleIdent (Maybe ExportSpec)
                     [ImportDecl] [Decl]

-- |Module pragma
data ModulePragma
  = LanguagePragma Pos [Extension]
  | OptionsPragma  Pos (Maybe Tool) String


-- |Export specification
data ExportSpec = Exporting Pos [Export]

-- |Single exported entity
data Export
  = Export         QualIdent
  | ExportTypeWith QualIdent [Ident]
  | ExportTypeAll  QualIdent
  | ExportModule   ModuleIdent

-- |Import declaration
data ImportDecl = ImportDecl Pos ModuleIdent Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)

-- |Flag to signal qualified import
type Qualified = Bool

-- |Import specification
data ImportSpec
  = Importing Pos [Import]
  | Hiding    Pos [Import]

-- |Single imported entity
data Import
  = Import         Ident
  | ImportTypeWith Ident [Ident]
  | ImportTypeAll  Ident

-- ---------------------------------------------------------------------------
-- Module interfaces
-- ---------------------------------------------------------------------------

-- | Module interface
data Interface = Interface ModuleIdent [IImportDecl] [IDecl]

-- |Interface import declaration
data IImportDecl = IImportDecl Pos ModuleIdent

-- |Arity of a function
type Arity = Int

-- |Interface declaration
data IDecl
  = IInfixDecl     Pos Infix Precedence  QualIdent
  | HidingDataDecl Pos QualIdent [Ident]
  | IDataDecl      Pos QualIdent [Ident] [ConstrDecl]  [Ident]
  | INewtypeDecl   Pos QualIdent [Ident] NewConstrDecl [Ident]
  | ITypeDecl      Pos QualIdent [Ident] TypeExpr
  | IFunctionDecl  Pos QualIdent Arity   TypeExpr

-- ---------------------------------------------------------------------------
-- Declarations (local or top-level)
-- ---------------------------------------------------------------------------

-- |Declaration in a module
data Decl
  = InfixDecl    Pos Infix (Maybe Precedence) [Ident]
  | DataDecl     Pos Ident [Ident] [ConstrDecl]
  | NewtypeDecl  Pos Ident [Ident] NewConstrDecl
  | TypeDecl     Pos Ident [Ident] TypeExpr
  | TypeSig      Pos [Ident] TypeExpr
  | FunctionDecl Pos Ident [Equation]
  | ForeignDecl  Pos CallConv (Maybe String) Ident TypeExpr
  | ExternalDecl Pos [Ident]
  | PatternDecl  Pos Pattern Rhs
  | FreeDecl     Pos [Ident]

-- ---------------------------------------------------------------------------
-- Infix declaration
-- ---------------------------------------------------------------------------

-- |Operator precedence
type Precedence = Int

-- |Fixity of operators
data Infix
  = InfixL -- ^ left-associative
  | InfixR -- ^ right-associative
  | Infix  -- ^ no associativity

-- |Constructor declaration for algebraic data types
data ConstrDecl
  = ConstrDecl Pos [Ident] Ident [TypeExpr]
  | ConOpDecl  Pos [Ident] TypeExpr Ident TypeExpr
  | RecordDecl Pos [Ident] Ident [FieldDecl]

-- |Constructor declaration for renaming types (newtypes)
data NewConstrDecl
  = NewConstrDecl Pos [Ident] Ident TypeExpr
  | NewRecordDecl Pos [Ident] Ident (Ident, TypeExpr)

-- |Declaration for labelled fields
data FieldDecl = FieldDecl Pos [Ident] TypeExpr

-- |Calling convention for C code
data CallConv
  = CallConvPrimitive
  | CallConvCCall

-- |Type expressions
data TypeExpr
  = ConstructorType QualIdent [TypeExpr]
  | VariableType    Ident
  | TupleType       [TypeExpr]
  | ListType        TypeExpr
  | ArrowType       TypeExpr TypeExpr
  | ParenType       TypeExpr

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- |Equation
data Equation = Equation Pos Lhs Rhs

-- |Left-hand-side of an `Equation` (function identifier and patterns)
data Lhs
  = FunLhs Ident [Pattern]
  | OpLhs  Pattern Ident Pattern
  | ApLhs  Lhs [Pattern]

-- |Right-hand-side of an `Equation`
data Rhs
  = SimpleRhs  Pos Expression [Decl]
  | GuardedRhs [CondExpr] [Decl]

-- |Conditional expression (expression conditioned by a guard)
data CondExpr = CondExpr Pos Expression Expression

-- |Literal
data Literal
  = Char   Char
  | Int    Ident  Int
  | Float  Float
  | String String

-- |Constructor term (used for patterns)
data Pattern
  = LiteralPattern     Literal
  | NegativePattern    Ident Literal
  | VariablePattern    Ident
  | ConstructorPattern QualIdent [Pattern]
  | InfixPattern       Pattern QualIdent Pattern
  | ParenPattern       Pattern
  | RecordPattern      QualIdent [Field Pattern]
  | TuplePattern       [Pattern]
  | ListPattern        [Pattern]
  | AsPattern          Ident Pattern
  | LazyPattern        Pattern
  | FunctionPattern    QualIdent [Pattern]
  | InfixFuncPattern   Pattern QualIdent Pattern

-- |Expression
data Expression
  = Literal           Literal
  | Variable          QualIdent
  | Constructor       QualIdent
  | Paren             Expression
  | Typed             Expression TypeExpr
  | Record            QualIdent [Field Expression]
  | RecordUpdate      Expression [Field Expression]
  | Tuple             [Expression]
  | List              [Expression]
  | ListCompr         Expression [Statement]
  | EnumFrom          Expression
  | EnumFromThen      Expression Expression
  | EnumFromTo        Expression Expression
  | EnumFromThenTo    Expression Expression Expression
  | UnaryMinus        Ident Expression
  | Apply             Expression Expression
  | InfixApply        Expression InfixOp Expression
  | LeftSection       Expression InfixOp
  | RightSection      InfixOp Expression
  | Lambda            [Pattern] Expression
  | Let               [Decl] Expression
  | Do                [Statement] Expression
  | IfThenElse        Expression Expression Expression
  | Case              CaseType Expression [Alt]

-- |Infix operation
data InfixOp
  = InfixOp     QualIdent
  | InfixConstr QualIdent

-- |Statement (used for do-sequence and list comprehensions)
data Statement
  = StmtExpr Expression
  | StmtDecl [Decl]
  | StmtBind Pattern Expression

-- |Type of case expressions
data CaseType
  = Rigid
  | Flex

-- |Single case alternative
data Alt = Alt Pos Pattern Rhs

-- |Record field
data Field a = Field Pos QualIdent a
