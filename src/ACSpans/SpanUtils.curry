module ACSpans.SpanUtils
  ( spanModule, spanDecl, spanConstr, spanNewConstr, spanField
  , spanTypeExpr, spanEquation, spanPat, spanRhs, spanCondExpr
  , spanExpr, spanLiteral, spanStmt, spanIdent, spanQIdent
  , spanSIdent, spanSQIdent
  , filterTS) where

import List (last)

import ACSpans.Ident ( Ident (..), ModuleIdent (..), QualIdent (..)
                     , SymQualIdent (..), SymIdent (..))
import ACSpans.Span
import ACSpans.SpanAST
import ACSpans.Token

-- -----------------------------------------------------------------------------
-- Utilities for computation of token stream with span information
-- -----------------------------------------------------------------------------

-- | Remove all comments from the token stream for synchronisation with AST
filterTS :: [(Span, Token)] -> [(Span, Token)]
filterTS = filter $ not . isComment . snd

-- | Is token a comment?
isComment :: Token -> Bool
isComment t = case t of
  LineComment   _ -> True
  NestedComment _ -> True
  _               -> False

-- -----------------------------------------------------------------------------
-- Computation of start positions
-- -----------------------------------------------------------------------------

-- | start position of a module
spModule :: Module -> Pos
spModule (Module _ mspan _ _ _ is ds) = case mspan of
  Just s            -> start s
  Nothing
    | not (null is) -> spImportDecl $ head is
    | not (null ds) -> spDecl $ head ds
    | otherwise     -> startPos

-- | start position of a declaration
spDecl :: Decl -> Pos
spDecl (InfixDecl       i _ _ _) = spInfix i
spDecl (DataDecl    s _ _ _ _ _) = start s
spDecl (NewtypeDecl   s _ _ _ _) = start s
spDecl (TypeDecl      s _ _ _ _) = start s
spDecl (TypeSig      syms _ _ _) = case syms of
  []    -> error "SpanUtils.spDecl: TypeSig with empty identifier list"
  sym:_ -> spSymIdent sym
spDecl (FunctionDecl        i _) = spIdent i
spDecl (ForeignDecl s _ _ _ _ _) = start s
spDecl (ExternalDecl   syms _ _) = case syms of
  []  -> error "SpanUtils.spDecl: ExternalDecl with empty identifier list"
  sym:_ -> spSymIdent sym
spDecl (PatternDecl         p _) = spPattern p
spDecl (FreeDecl         is _ _) = case is of
  []  -> error "SpanUtils.spDecl: FreeDecl with empty identifier list"
  i:_ -> spIdent i

-- | start position of a constructor declaration
spConstr :: ConstrDecl -> Pos
spConstr (ConstrDecl       _ i _) = spIdent i
spConstr (ConOpDecl     _ ty _ _) = spTypeExpr ty
spConstr (RecordDecl _ i _ _ _ _) = spIdent i

-- | start position of a newtype constructor declaration
spNewConstr :: NewConstrDecl -> Pos
spNewConstr (NewConstrDecl     _ i _) = spIdent i
spNewConstr (NewRecordDecl _ i _ _ _) = spIdent i

-- | start position of a field declaration
spField :: FieldDecl -> Pos
spField (FieldDecl is _ _ _) = case is of
  []  -> error "SpanUtils.spField: FieldDecl with empty identifier list"
  i:_ -> spIdent i

-- | start position of a type expression
spTypeExpr :: TypeExpr -> Pos
spTypeExpr (ConstructorType mspan c _ _) = case mspan of
  Just s  -> start s
  Nothing -> spQIdent c
spTypeExpr (VariableType    i) = spIdent i
spTypeExpr (TupleType s _ _ _) = start s
spTypeExpr (ListType    s _ _) = start s
spTypeExpr (ArrowType  ty _ _) = spTypeExpr ty
spTypeExpr (ParenType   s _ _) = start s

-- | start position of an equation
spEquation :: Equation -> Pos
spEquation (Equation lhs _) = spLhs lhs

-- | start position of a pattern
spPattern :: Pattern -> Pos
spPattern (LiteralPattern           l) = spLiteral l
spPattern (NegativePattern        i _) = spIdent i
spPattern (VariablePattern          i) = spIdent i
spPattern (ConstructorPattern    qi _) = spQIdent qi
spPattern (InfixPattern         p _ _) = spPattern p
spPattern (ParenPattern         s _ _) = start s
spPattern (RecordPattern   qi _ _ _ _) = spQIdent qi
spPattern (TuplePattern       s _ _ _) = start s
spPattern (ListPattern        s _ _ _) = start s
spPattern (AsPattern            i _ _) = spIdent i
spPattern (LazyPattern            s _) = start s
spPattern (FunctionPattern       qi _) = spQIdent qi
spPattern (InfixFuncPattern     p _ _) = spPattern p

-- | start position of a lhs
spLhs :: Lhs -> Pos
spLhs (FunLhs si _) = spSymIdent si
spLhs (OpLhs p _ _) = spPattern p
spLhs (ApLhs lhs _) = spLhs lhs

-- | start position of a rhs
spRhs :: Rhs -> Pos
spRhs (SimpleRhs      _ e _ _) = spExpr e
spRhs (GuardedRhs _ ces _ _ _) = spCondExpr $ head ces

-- | start position of a conditional expression
spCondExpr :: CondExpr -> Pos
spCondExpr (CondExpr e _ _) = spExpr e

-- | start position of an expression
spExpr :: Expression -> Pos
spExpr (Literal                    l) = spLiteral l
spExpr (Variable                 sqi) = spSymQualIdent sqi
spExpr (Constructor              sqi) = spSymQualIdent sqi
spExpr (Paren                  s _ _) = start s
spExpr (Typed                  e _ _) = spExpr e
spExpr (Record            qi _ _ _ _) = spQIdent qi
spExpr (RecordUpdate       e _ _ _ _) = spExpr e
spExpr (Tuple                s _ _ _) = start s
spExpr (List                 s _ _ _) = start s
spExpr (ListCompr        s _ _ _ _ _) = start s
spExpr (EnumFrom             s _ _ _) = start s
spExpr (EnumFromThen     s _ _ _ _ _) = start s
spExpr (EnumFromTo         s _ _ _ _) = start s
spExpr (EnumFromThenTo s _ _ _ _ _ _) = start s
spExpr (UnaryMinus               i _) = spIdent i
spExpr (Apply                    e _) = spExpr e
spExpr (InfixApply             e _ _) = spExpr e
spExpr (LeftSection          s _ _ _) = start s
spExpr (RightSection         s _ _ _) = start s
spExpr (Lambda               s _ _ _) = start s
spExpr (Let                  s _ _ _) = start s
spExpr (Do                     s _ _) = start s
spExpr (IfThenElse       s _ _ _ _ _) = start s
spExpr (Case               _ s _ _ _) = start s

-- | start position of a literal
spLiteral :: Literal -> Pos
spLiteral (Char   s _) = start s
spLiteral (Int    s _) = start s
spLiteral (Float  s _) = start s
spLiteral (String s _) = start s

-- start position of a statement
spStmt :: Statement -> Pos
spStmt (StmtExpr     e) = spExpr e
spStmt (StmtDecl   s _) = start s
spStmt (StmtBind _ p _) = spPattern p

-- | start position of an identifier
spIdent :: Ident -> Pos
spIdent = start . idSpan

-- | start position of a qualified identifier
spQIdent :: QualIdent -> Pos
spQIdent = start . idSpan . qidIdent

-- | start position of a parenthesized identifier
spSymIdent :: SymIdent -> Pos
spSymIdent (SymIdent ms i _) = case ms of
  Just s  -> start s
  Nothing -> spIdent i

-- | start position of a parenthesized qualified identifier
spSymQualIdent :: SymQualIdent -> Pos
spSymQualIdent (SymQualIdent ms qi _) = case ms of
  Just s  -> start s
  Nothing -> spQIdent qi

-- | start position of a fixity
spInfix :: Infix -> Pos
spInfix (InfixL s) = start s
spInfix (InfixR s) = start s
spInfix (Infix  s) = start s

-- | start position of an import declaration
spImportDecl :: ImportDecl -> Pos
spImportDecl (ImportDecl s _ _ _ _ _ _) = start s

-- -----------------------------------------------------------------------------
-- Computation of end positions
-- -----------------------------------------------------------------------------

-- | end position of a module
epModule :: Module -> Pos
epModule (Module _ _ _ mspan mes is ds)
  | not (null ds) = epDecl $ last ds
  | not (null is) = epImportDecl $ last is
  | otherwise = case mes of
                  Just spec -> epExportSpec spec
                  Nothing   -> case mspan of
                                 Just s  -> end s
                                 Nothing -> startPos

-- | end position of a declaration
epDecl :: Decl -> Pos
epDecl (InfixDecl      _ _ ops _)
  | null ops                      = error "SpanUtils.epDecl: InfixDecl with empty infix operator list"
  | otherwise                     = epSymIdent $ last ops
epDecl (DataDecl    _ i _ _ cs _)
  | null cs                       = epIdent i
  | otherwise                     = epConstr $ last cs
epDecl (NewtypeDecl   _ _ _ _ nc) = epNewConstr nc
epDecl (TypeDecl      _ _ _ _ ty) = epTypeExpr ty
epDecl (TypeSig         _ _ _ ty) = epTypeExpr ty
epDecl (FunctionDecl       _ eqs)
  | null eqs                      = error "SpanUtils.epDecl: FunctionDecl with empty equation list"
  | otherwise                     = epEquation $ last eqs
epDecl (ForeignDecl _ _ _ _ _ ty) = epTypeExpr ty
epDecl (ExternalDecl       _ _ s) = end s
epDecl (PatternDecl        _ rhs) = epRhs rhs
epDecl (FreeDecl           _ _ s) = end s

-- | end position of a constructor declaration
epConstr :: ConstrDecl -> Pos
epConstr (ConstrDecl     _ c tys)
  | null tys                      = epIdent c
  | otherwise                     = epTypeExpr $ last tys
epConstr (ConOpDecl     _ _ _ ty) = epTypeExpr ty
epConstr (RecordDecl _ _ _ _ _ s) = end s

-- | end position of a newtype constructor declaration
epNewConstr :: NewConstrDecl -> Pos
epNewConstr (NewConstrDecl    _ _ ty) = epTypeExpr ty
epNewConstr (NewRecordDecl _ _ _ _ s) = end s

-- | end position of a field declaration
epField :: FieldDecl -> Pos
epField (FieldDecl _ spans _ ty) = case spans of
  [s] -> end s
  _   -> epTypeExpr ty

-- | end position of a type expression
epTypeExpr :: TypeExpr -> Pos
epTypeExpr (ConstructorType _ c tys mspan) = case mspan of
  Just s  -> end s
  Nothing
    | null tys  -> epQIdent c
    | otherwise -> epTypeExpr $ last tys
epTypeExpr (VariableType    i) = epIdent i
epTypeExpr (TupleType _ _ _ s) = end s
epTypeExpr (ListType    _ _ s) = end s
epTypeExpr (ArrowType  _ _ ty) = epTypeExpr ty
epTypeExpr (ParenType   _ _ s) = end s

-- | end position of an equation
epEquation :: Equation -> Pos
epEquation (Equation _ rhs) = epRhs rhs

-- | end position of a pattern
epPattern :: Pattern -> Pos
epPattern (LiteralPattern         l) = epLiteral l
epPattern (NegativePattern      _ l) = epLiteral l
epPattern (VariablePattern        i) = epIdent i
epPattern (ConstructorPattern qi ps)
  | null ps                          = epQIdent qi
  | otherwise                        = epPattern $ last ps
epPattern (InfixPattern       _ _ p) = epPattern p
epPattern (ParenPattern       _ _ s) = end s
epPattern (RecordPattern  _ _ _ _ s) = end s
epPattern (TuplePattern     _ _ _ s) = end s
epPattern (ListPattern      _ _ _ s) = end s
epPattern (AsPattern          _ _ p) = epPattern p
epPattern (LazyPattern          _ p) = epPattern p
epPattern (FunctionPattern    qi ps)
  | null ps                          = epQIdent qi
  | otherwise                        = epPattern $ last ps
epPattern (InfixFuncPattern   _ _ p) = epPattern p

-- | end position of rhs
epRhs :: Rhs -> Pos
epRhs (SimpleRhs _ e _ ds)
  | null ds   = epExpr e
  | otherwise = epDecl $ last ds
epRhs (GuardedRhs _ ces _ _ ds)
  | null ds   = if null ces then error "SpanUtils.epRhs: GuardedRhs with empty conditional expression list"
                            else epCondExpr $ last ces
  | otherwise = epDecl $ last ds

-- | end position of a conditional expression
epCondExpr :: CondExpr -> Pos
epCondExpr (CondExpr _ _ e) = epExpr e

-- | end position of an expression
epExpr :: Expression -> Pos
epExpr (Literal                    l) = epLiteral l
epExpr (Variable                 sqi) = epSymQualIdent sqi
epExpr (Constructor              sqi) = epSymQualIdent sqi
epExpr (Paren                  _ _ s) = end s
epExpr (Typed                 _ _ ty) = epTypeExpr ty
epExpr (Record             _ _ _ _ s) = end s
epExpr (RecordUpdate       _ _ _ _ s) = end s
epExpr (Tuple                _ _ _ s) = end s
epExpr (List                 _ _ _ s) =  end s
epExpr (ListCompr        _ _ _ _ _ s) = end s
epExpr (EnumFrom             _ _ _ s) = end s
epExpr (EnumFromThen     _ _ _ _ _ s) = end s
epExpr (EnumFromTo         _ _ _ _ s) = end s
epExpr (EnumFromThenTo _ _ _ _ _ _ s) = end s
epExpr (UnaryMinus               _ e) = epExpr e
epExpr (Apply                    _ e) = epExpr e
epExpr (InfixApply             _ _ e) = epExpr e
epExpr (LeftSection          _ _ _ s) = end s
epExpr (RightSection         _ _ _ s) = end s
epExpr (Lambda               _ _ _ e) = epExpr e
epExpr (Let                  _ _ _ e) = epExpr e
epExpr (Do                     _ _ e) = epExpr e
epExpr (IfThenElse       _ _ _ _ _ e) = epExpr e
epExpr (Case            _ _ _ _ alts)
  | null alts                         = error "SpanUtils.epExpr: Case with empty alternative list"
  | otherwise                         = epAlt $ last alts

-- | end position of a literal
epLiteral :: Literal -> Pos
epLiteral (Char   s _) = end s
epLiteral (Int    s _) = end s
epLiteral (Float  s _) = end s
epLiteral (String s _) = end s

-- | end position of a statement
epStmt :: Statement -> Pos
epStmt (StmtExpr     e) = epExpr e
epStmt (StmtDecl  _ ds)
  | null ds             = error "SpanUtils.epStmt: StmtDecl with empty declaration list"
  | otherwise           = epDecl $ last ds
epStmt (StmtBind _ _ e) = epExpr e

-- | end position of an identifier
epIdent :: Ident -> Pos
epIdent = end . idSpan

-- | end position of a qualified identifier
epQIdent :: QualIdent -> Pos
epQIdent = end . idSpan . qidIdent

-- | end position of a parenthesized identifier
epSymIdent :: SymIdent -> Pos
epSymIdent (SymIdent _ i ms) = case ms of
  Just s  -> end s
  Nothing -> epIdent i

-- | end position of a parenthesized qualified identifier
epSymQualIdent :: SymQualIdent -> Pos
epSymQualIdent (SymQualIdent _ qi ms) = case ms of
  Just s  -> end s
  Nothing -> epQIdent qi

-- | end position of a module identifier
epMIdent :: ModuleIdent -> Pos
epMIdent = end . midSpan

-- | end position of an import declaration
epImportDecl :: ImportDecl -> Pos
epImportDecl (ImportDecl _ _ m _ _ mm mspec) = case mspec of
  Just spec -> epImportSpec spec
  Nothing   -> case mm of
                 Just mi -> epMIdent mi
                 Nothing -> epMIdent m

-- | end position of an import spec
epImportSpec :: ImportSpec -> Pos
epImportSpec (Importing _ _ _ s) = end s
epImportSpec (Hiding  _ _ _ _ s) = end s

-- | end position of an export spec
epExportSpec :: ExportSpec -> Pos
epExportSpec (Exporting _ _ _ s) = end s

-- | end position of an alternative
epAlt :: Alt -> Pos
epAlt (Alt _ rhs) = epRhs rhs

-- -----------------------------------------------------------------------------
-- Computation of spans
-- -----------------------------------------------------------------------------

-- | span of a module
spanModule :: Module -> Span
spanModule p = (spModule p, epModule p)

-- | span of a declaration
spanDecl :: Decl -> Span
spanDecl d = (spDecl d, epDecl d)

-- | span of a constructor declaration
spanConstr :: ConstrDecl -> Span
spanConstr c = (spConstr c, epConstr c)

-- | span of a newtype constructor declaration
spanNewConstr :: NewConstrDecl -> Span
spanNewConstr nc = (spNewConstr nc, epNewConstr nc)

-- | span of a field declaration
spanField :: FieldDecl -> Span
spanField f = (spField f, epField f)

-- | span of a type expression
spanTypeExpr :: TypeExpr -> Span
spanTypeExpr ty = (spTypeExpr ty, epTypeExpr ty)

-- | span of an equation
spanEquation :: Equation -> Span
spanEquation eq = (spEquation eq, epEquation eq)

-- | span of a pattern
spanPat :: Pattern -> Span
spanPat p = (spPattern p, epPattern p)

-- | span of a rhs
spanRhs :: Rhs -> Span
spanRhs rhs = (spRhs rhs, epRhs rhs)

-- | span of conditional expression
spanCondExpr :: CondExpr -> Span
spanCondExpr cexp = (spCondExpr cexp, epCondExpr cexp)

-- | span of an expression
spanExpr :: Expression -> Span
spanExpr e = (spExpr e, epExpr e)

-- | span of a literal
spanLiteral :: Literal -> Span
spanLiteral l = (spLiteral l, epLiteral l)

-- | span of a statement
spanStmt :: Statement -> Span
spanStmt s = (spStmt s, epStmt s)

-- | span of an identifier
spanIdent :: Ident -> Span
spanIdent = idSpan

-- | span of a qualified identifier
spanQIdent :: QualIdent -> Span
spanQIdent = idSpan . qidIdent

-- | span of a parenthesized identifier
spanSIdent :: SymIdent -> Span
spanSIdent sym = (spSymIdent sym, epSymIdent sym)

-- | span of a parenthesized qualified identifier
spanSQIdent :: SymQualIdent -> Span
spanSQIdent sym = (spSymQualIdent sym, epSymQualIdent sym)
