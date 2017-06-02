{- |
    Module      :  ACSpans.AddSpans
    Description :  Extension of Abstract Syntax Tree

    This module contains functions for extending a curry AST with span
    information.
    The structure is based on the structure of an AST.
    The add-span-functions depict a traversion of the AST.
-}
module ACSpans.AddSpans (apModule) where

import Char  (toUpper)
import List  (last)
import Maybe (fromMaybe)

import           ACSpans.ASM
import           ACSpans.AST                                    as AST
import qualified ACSpans.Ident                                  as I
import           ACSpans.PositionUtils (moveColBy)
import           ACSpans.Span (Span, virtualSpan, isVirtualPos)
import qualified ACSpans.SpanAST                                as SpanAST
import           ACSpans.Token

-- |Set span of ModuleIdent
setMIdSpan :: Span -> AST.ModuleIdent -> I.ModuleIdent
setMIdSpan s m = let qs = midQualifiers m
                 in I.ModuleIdent s qs

-- |Set span of Ident
setIdSpan :: Span -> AST.Ident -> I.Ident
setIdSpan s i = let n = AST.idName i
                    u = AST.idUnique i
                in I.Ident s n u

-- |Set span of QualIdent
setQIdSpan :: Span -> AST.QualIdent -> I.QualIdent
setQIdSpan s q = let i = AST.qidIdent q in case AST.qidModule q of
  Just m  -> I.QualIdent (Just (setMIdSpan s m)) (setIdSpan s i)
  Nothing -> I.QualIdent Nothing (setIdSpan s i)

-- ----------------------------------------------------------------------------
-- Modules
-- ----------------------------------------------------------------------------

-- |Add span information to module
apModule :: AST.Module -> ASM SpanAST.Module
apModule (AST.Module ps mi mes is ds) =
  mapM apPragma         ps                             >+= \ ps'  ->
  maybeTokenSpan         KW_module                      >+= \ mpm  ->
  (case mpm of
     Nothing -> returnP (Nothing, Nothing, Nothing)
     Just _  -> (,,) <$> maybeOneOf (Id (moduleName mi))(QId (moduleName mi))
                     <*> optional apExportSpec mes
                     <*> maybeTokenSpan KW_where) >+= \(mpn, mes', mpw) ->
  mapM apImportDecl     is                             >+= \ is'  ->
  mapM apDecl           ds                             >+= \ ds'  ->
  let mi' = setMIdSpan (fromMaybe virtualSpan mpn) mi
  in returnP (SpanAST.Module ps' mpm mi' mpw mes' is' ds')

-- |Add span information to pragma
apPragma :: AST.ModulePragma -> ASM SpanAST.ModulePragma
apPragma (AST.LanguagePragma _ exts) =
  tokenSpan                    PragmaLanguage >+= \ pl           ->
  sepBy apExtension (tokenSpan Comma) exts    >+= \ (pexts, pss) ->
  tokenSpan                    PragmaEnd      >+= \ pr           ->
  returnP (SpanAST.LanguagePragma pl pexts pss pr)
apPragma (AST.OptionsPragma _ mt s) =
  getTokenSpan        >+= \ (pl, op) ->
  tokenSpan PragmaEnd >+= \ pr       ->
  ensure ((getMbTool op) == (showMbTool mt))
         (SpanAST.OptionsPragma pl (showMbTool mt) s pr)

-- |Extract (Maybe Tool) from Token PragmaOptions
getMbTool :: Token -> (Maybe String)
getMbTool tok = case tok of
  (PragmaOptions mt _) -> case mt of
                           Nothing -> Nothing
                           Just t  -> Just (map toUpper t)
  _                    -> error "Next token is not PragmaOptions."

-- |Show (Maybe Tool) to convert it into (Maybe String)
showMbTool :: (Maybe Tool) -> (Maybe String)
showMbTool mt = case mt of
  Nothing -> Nothing
  Just t  -> Just (show t)

-- |Convert extensions
apExtension :: AST.Extension -> ASM SpanAST.Extension
apExtension (AST.KnownExtension p e)
  = getTokenSpan >+= \ (_, Id e') ->
    ensure (show e == e') (SpanAST.KnownExtension (p, moveColBy p (length (show e) - 1)) e)
-- can never be reached because of compiler error
-- "Unknown language extension: "LanguageOfDoom"
apExtension (AST.UnknownExtension p s)
  = getTokenSpan >+= \ (_, StringTok s') ->
    ensure (show s == s') (SpanAST.UnknownExtension (p, moveColBy p (length s - 1)) s)

-- |Add span information to export specification
apExportSpec :: AST.ExportSpec -> ASM SpanAST.ExportSpec
apExportSpec (AST.Exporting _ exs)
  = parens (sepBy apExport (tokenSpan Comma) exs)
      >+= \ (pl, (exs', pcs), pr) ->
    returnP (SpanAST.Exporting pl exs' pcs pr)

-- |Add span information to export
apExport :: AST.Export -> ASM SpanAST.Export
apExport (AST.Export qi) =
  apSymQualIdent qi >+= \ qi' ->
  returnP (SpanAST.Export qi')
apExport (AST.ExportTypeWith qi is) =
  apQualIdent2  qi                    >+= \ qi'        ->
  tokenSpan      LeftParen             >+= \ pl         ->
  (sepBy apIdent (tokenSpan Comma) is) >+= \ (is', pcs) ->
  tokenSpan      RightParen            >+= \ pr         ->
  returnP (SpanAST.ExportTypeWith qi' pl is' pcs pr)
apExport (AST.ExportTypeAll qi) =
  apQualIdent2 qi         >+= \ qi' ->
  tokenSpan     LeftParen  >+= \ pl  ->
  tokenSpan     DotDot     >+= \ pdd ->
  tokenSpan     RightParen >+= \ pr  ->
  returnP (SpanAST.ExportTypeAll qi' pl pdd pr)
apExport (AST.ExportModule mi) =
  tokenSpan KW_module >+= \ pm  ->
  apModIdent      mi >+= \ mi' ->
  returnP (SpanAST.ExportModule pm mi')

-- |Add span information to import declaration
apImportDecl :: AST.ImportDecl -> ASM SpanAST.ImportDecl
apImportDecl (AST.ImportDecl p mi q mmi mis)
  -- do nothing with implicit import of prelude
  | isVirtualPos p
    = let mi'  = setMIdSpan virtualSpan mi
          mmi' = maybe Nothing (\m -> Just (setMIdSpan virtualSpan m)) mmi
      in returnP (SpanAST.ImportDecl virtualSpan Nothing mi' Nothing q mmi' Nothing)
  | otherwise = tokenSpan      KW_import         >+= \ pi   ->
                maybeTokenSpan Id_qualified      >+= \ mpq  ->
                apModIdent    mi                >+= \ mi'  ->
                maybeTokenSpan Id_as             >+= \ mpa  ->
                optional      apModIdent mmi    >+= \ mmi' ->
                optional      apImportSpec mis  >+= \ mis' ->
                returnP (SpanAST.ImportDecl pi mpq mi' mpa q mmi' mis')

-- |Add span information to Import specification
apImportSpec :: AST.ImportSpec -> ASM SpanAST.ImportSpec
apImportSpec (AST.Importing _ is) =
  parens (sepBy apImport (tokenSpan Comma) is) >+= \ (pl, (pis, pcs), pr) ->
  returnP (SpanAST.Importing pl pis pcs pr)
apImportSpec (AST.Hiding _ is) =
  tokenSpan Id_hiding                           >+= \ ph                   ->
  parens  (sepBy apImport (tokenSpan Comma) is) >+= \ (pl, (pis, pcs), pr) ->
  returnP (SpanAST.Hiding ph pl pis pcs pr)

-- |Add span information to single imported entity
apImport :: AST.Import -> ASM SpanAST.Import
apImport (AST.Import i) = SpanAST.Import <$> apSymIdent i
apImport (AST.ImportTypeWith i is) =
  apIdent i                                   >+= \ i'                   ->
  parens  (sepBy apIdent (tokenSpan Comma) is) >+= \ (pl, (pis, pcs), pr) ->
  returnP (SpanAST.ImportTypeWith i' pl pis pcs pr)
apImport (AST.ImportTypeAll i) =
  apIdent i                 >+= \ i'           ->
  parens  (tokenSpan DotDot) >+= \ (pl, pd, pr) ->
  returnP (SpanAST.ImportTypeAll i' pl pd pr)

-- -----------------------------------------------------------------------------
-- Declarations (local or top-level)
-- -----------------------------------------------------------------------------

-- |Add span information to declaration
apDecl :: AST.Decl -> ASM SpanAST.Decl
apDecl (AST.InfixDecl _ inf mprec is) =
  apInfix        inf                    >+= \ inf'       ->
  optional apInt mprec                  >+= \ mprec'     ->
  sepBy apSymIdent (tokenSpan Comma) is >+= \ (ops, cps) ->
  returnP (SpanAST.InfixDecl inf' mprec' ops cps)
apDecl (AST.DataDecl _ i is cds) =
  tokenSpan KW_data                      >+= \ pd          ->
  apIdent  i                            >+= \ i'          ->
  mapM apIdent is                       >+= \ is'         ->
  maybeTokenSpan Equals                  >+= \ pe          ->
  sepBy apConstrDecl (tokenSpan Bar) cds >+= \ (pcds, pss) ->
  returnP (SpanAST.DataDecl pd i' is' pe pcds pss)
apDecl (AST.NewtypeDecl _ i is ncd) =
  tokenSpan        KW_newtype >+= \ pn   ->
  apIdent         i          >+= \ i'   ->
  mapM apIdent    is         >+= \ is'  ->
  tokenSpan        Equals     >+= \ pe   ->
  apNewConstrDecl ncd        >+= \ ncd' ->
  returnP (SpanAST.NewtypeDecl pn i' is' pe ncd')
apDecl (AST.TypeDecl _ i is te) =
  tokenSpan   KW_type >+= \ pt  ->
  apIdent    i       >+= \ i'  ->
  mapM apIdent is    >+= \ is' ->
  tokenSpan   Equals  >+= \ pe  ->
  apTypeExpr te      >+= \ te' ->
  returnP (SpanAST.TypeDecl pt i' is' pe te')
apDecl (AST.FunctionDecl _ i eqs) =
  let p  = idPosition i
      i' = setIdSpan (p, moveColBy p (length (idName i) - 1)) i
  in mapM apEquation eqs >+= \ eqs' ->
  returnP (SpanAST.FunctionDecl i' eqs')
apDecl (AST.TypeSig _ is te) =
  sepBy apSymIdent (tokenSpan Comma) is >+= \ (is', pcs) ->
  tokenSpan DoubleColon                 >+= \ pdc        ->
  apTypeExpr te                         >+= \ te'        ->
  returnP (SpanAST.TypeSig is' pcs pdc te')
apDecl (AST.ForeignDecl _ cc mbLib i te) =
  tokenSpan   KW_foreign    >+= \ pf     ->
  apCallConv cc            >+= \ cc'    ->
  optional apString mbLib  >+= \ mbLib' ->
  apSymIdent i             >+= \ i'     ->
  tokenSpan   DoubleColon   >+= \ pd     ->
  apTypeExpr te            >+= \ te'    ->
  returnP (SpanAST.ForeignDecl pf cc' mbLib' i' pd te')
apDecl (AST.ExternalDecl _ is) =
  sepBy apSymIdent (tokenSpan Comma) is >+= \ (is', pss) ->
  tokenSpan KW_external                 >+= \ pe         ->
  returnP (SpanAST.ExternalDecl is' pss pe)
apDecl (AST.PatternDecl _ pat rhs) =
  apPat    pat           >+= \ pat' ->
  apRhs    rhs Equals    >+= \ rhs' ->
  returnP (SpanAST.PatternDecl pat' rhs')
apDecl (AST.FreeDecl _ is) =
  sepBy apIdent (tokenSpan Comma) is >+= \ (is', pss) ->
  tokenSpan KW_free                  >+= \ pf         ->
  returnP (SpanAST.FreeDecl is' pss pf)


-- ----------------------------------------------------------------------------
-- Infix declaration
-- ----------------------------------------------------------------------------

-- |Add span information to Infix
apInfix :: AST.Infix -> ASM SpanAST.Infix
apInfix AST.InfixL = SpanAST.InfixL <$> tokenSpan KW_infixl
apInfix AST.InfixR = SpanAST.InfixR <$> tokenSpan KW_infixr
apInfix AST.Infix  = SpanAST.Infix  <$> tokenSpan KW_infix

-- |Add span information to constructor declaration for algebraic data types
apConstrDecl :: AST.ConstrDecl -> ASM SpanAST.ConstrDecl
apConstrDecl (AST.ConstrDecl _ is i tes) =
  mapM apIdent    is  >+= \ is' ->
  apIdent         i   >+= \ i'  ->
  mapM apTypeExpr tes >+= \tes' ->
  returnP (SpanAST.ConstrDecl is' i' tes')
apConstrDecl (AST.ConOpDecl _ is te1 i te2) =
  mapM apIdent is  >+= \ is'  ->
  apTypeExpr   te1 >+= \ te1' ->
  apIdent      i   >+= \ i'   ->
  apTypeExpr   te2 >+= \ te2' ->
  returnP (SpanAST.ConOpDecl is' te1' i' te2')
apConstrDecl (AST.RecordDecl _ is i fds) =
  mapM apIdent                               is   >+= \ is'                   ->
  apIdent                                    i    >+= \ i'                    ->
  braces (sepBy apFieldDecl (tokenSpan Comma) fds) >+= \ (pl, (pfds, pss), pr) ->
  returnP (SpanAST.RecordDecl is' i' pl pfds pss pr)

-- |Add span information to constructor declaration for renaming types (newtypes)
apNewConstrDecl :: AST.NewConstrDecl -> ASM SpanAST.NewConstrDecl
apNewConstrDecl (AST.NewConstrDecl _ is i te) =
  mapM apIdent is         >+= \ is' ->
  apIdent      i          >+= \ i'  ->
  apTypeExpr   te         >+= \ te' ->
  returnP (SpanAST.NewConstrDecl is' i' te')
apNewConstrDecl (AST.NewRecordDecl _ is i1 (i2, te)) =
  mapM apIdent            is     >+= \ is'                      ->
  apIdent                 i1     >+= \ i1'                      ->
  braces (apNewRecordDecl i2 te) >+= \ (pl, (i2', pc, te'), pr) ->
  returnP (SpanAST.NewRecordDecl is' i1' pl (i2', pc, te') pr)
  where
    apNewRecordDecl i2_ te_ =
      apIdent      i2_          >+= \ i2' ->
      tokenSpan     DoubleColon  >+= \ pc  ->
      apTypeExpr   te_          >+= \ te' ->
      returnP (i2', pc, te')

-- |Add span information to declaration for labelled fields
apFieldDecl :: AST.FieldDecl -> ASM SpanAST.FieldDecl
apFieldDecl (AST.FieldDecl _ is te) =
  sepBy apIdent (tokenSpan Comma) is   >+= \ (is', pss) ->
  tokenSpan                DoubleColon >+= \ p          ->
  apTypeExpr              te          >+= \ te'        ->
  returnP (SpanAST.FieldDecl is' pss p te')

-- |Add span information to calling convention for C code
apCallConv :: AST.CallConv -> ASM SpanAST.CallConv
apCallConv (AST.CallConvPrimitive) =
  tokenSpan Id_primitive >+= \ p ->
  returnP (SpanAST.CallConvPrimitive p)
apCallConv (AST.CallConvCCall) =
  tokenSpan Id_ccall >+= \ p ->
  returnP (SpanAST.CallConvCCall p)

-- |Add span information to type expressions
apTypeExpr :: AST.TypeExpr -> ASM SpanAST.TypeExpr
apTypeExpr (AST.ConstructorType qi tes) =
    maybeParens (apConstructorType qi tes) >+= \ (mpl, (qi', tes'), mpr) ->
    returnP (SpanAST.ConstructorType mpl qi' tes' mpr)
  where
    apConstructorType qi_ tes_ =
      apQualIdent     qi_  >+= \ qi'  ->
      mapM apTypeExpr tes_ >+= \ tes' ->
      returnP (qi', tes')
apTypeExpr (AST.VariableType x) = SpanAST.VariableType <$> apIdent x
apTypeExpr (AST.TupleType tes) =
  parens (sepBy apTypeExpr (tokenSpan Comma) tes) >+= \ (pl, (tes', pss), pr) ->
  returnP (SpanAST.TupleType pl tes' pss pr)
apTypeExpr (AST.ListType te) =
  brackets (apTypeExpr te) >+= \ (pl, te', pr) ->
  returnP (SpanAST.ListType pl te' pr)
apTypeExpr (AST.ArrowType te1 te2) =
  apTypeExpr te1        >+= \ te1' ->
  tokenSpan   RightArrow >+= \ pa   ->
  apTypeExpr te2        >+= \ te2' ->
  returnP (SpanAST.ArrowType te1' pa te2')
apTypeExpr (AST.ParenType te) =
  parens (apTypeExpr te) >+= \ (pl, te', pr) ->
  returnP (SpanAST.ParenType pl te' pr)

-- ----------------------------------------------------------------------------
-- Functions
-- ----------------------------------------------------------------------------

-- |Add span information to equation
apEquation :: AST.Equation -> ASM SpanAST.Equation
apEquation (AST.Equation _ lhs rhs) =
  apLhs       lhs        >+= \ lhs' ->
  apRhs       rhs Equals >+= \ rhs' ->
  returnP (SpanAST.Equation lhs' rhs')

-- |Add span information to left-hand side
apLhs :: AST.Lhs -> ASM SpanAST.Lhs
apLhs (AST.FunLhs f ps) =
  apSymIdent f  >+= \ f'  ->
  mapM apPat ps >+= \ ps' ->
  returnP (SpanAST.FunLhs f' ps')
apLhs (AST.OpLhs pat1 si pat2) =
  apPat     pat1 >+= \ pat1' ->
  apSymIdent si  >+= \ si'   ->
  apPat     pat2 >+= \ pat2' ->
  returnP (SpanAST.OpLhs pat1' si' pat2')
apLhs (AST.ApLhs lhs pats) =
  apLhs      lhs  >+= \ lhs'  ->
  mapM apPat pats >+= \ pats' ->
  returnP (SpanAST.ApLhs lhs' pats')

-- |Add span information to right-hand side
apRhs :: AST.Rhs -> Token -> ASM SpanAST.Rhs
apRhs (AST.SimpleRhs _ e ds) t =
  tokenSpan     t  >+= \ pe        ->
  apExpr       e  >+= \ e'        ->
  apLocalDecls ds >+= \ (mp, ds') ->
  returnP (SpanAST.SimpleRhs pe e' mp ds')
apRhs (AST.GuardedRhs ces ds) _ =
  tokenSpan                   Bar      >+= \ pb          ->
  sepBy apCondExpr (tokenSpan Bar) ces >+= \ (ces', pss) ->
  apLocalDecls               ds       >+= \ (mp, ds')   ->
  returnP (SpanAST.GuardedRhs pb ces' pss mp ds')

apLocalDecls :: [AST.Decl] -> ASM (Maybe Span, [SpanAST.Decl])
apLocalDecls [] = returnP (Nothing, [])
apLocalDecls ds@(_:_) = tokenSpan KW_where >+= \ mpw  ->
                        mapM apDecl ds    >+= \ ds'  ->
                        returnP (Just mpw, ds')

-- |Add span information to Conditional expression (expression conditioned by a guard)
apCondExpr :: AST.CondExpr -> ASM SpanAST.CondExpr
apCondExpr (AST.CondExpr _ e1 e2) =
  apExpr        e1                >+= \ e1' ->
  tokenSpanOneOf Equals RightArrow >+= \ pe  ->
  apExpr        e2                >+= \ e2' ->
  returnP (SpanAST.CondExpr e1' pe e2')

-- |Add span information to Int
apInt :: Int -> ASM (Span, Int)
apInt i = getTokenSpan >+= \ (p, IntTok i') -> ensure (i == i') (p, i)

-- |Add span information to String
apString :: String -> ASM (Span, String)
apString s = getTokenSpan >+= \ (p, StringTok s') -> ensure (s == s') (p, s)

-- |Add span information to literal
apLit :: AST.Literal -> ASM SpanAST.Literal
apLit (AST.Char   c') = getTokenSpan >+= \ (p, CharTok   c) ->
                            ensure (c == c') (SpanAST.Char   p c)
apLit (AST.Int _  i') = getTokenSpan >+= \ (p, IntTok    i) ->
                            ensure (i == i') (SpanAST.Int    p i)
apLit (AST.Float  f') = getTokenSpan >+= \ (p, FloatTok  f) ->
                            ensure (f == f') (SpanAST.Float  p f)
apLit (AST.String s') = getTokenSpan >+= \ (p, StringTok s) ->
                            ensure (s == s') (SpanAST.String p s)

-- |Add span information to identifier
apIdent :: AST.Ident -> ASM I.Ident
apIdent i = getTokenSpan >+= \ (p, t) -> case t of
  Id       i' -> ensure (idName i == i')   (setIdSpan p i)
  -- no `ensure` here because a symbol like `\\` will be
  -- escaped as `\\\\` and won't be recognized
  Sym      _  -> returnP (setIdSpan p i)
  SymDot      -> ensure (idName i == ".")  (setIdSpan p i)
  SymMinus    -> ensure (idName i == "-")  (setIdSpan p i)
  SymMinusDot -> ensure (idName i == "-.") (setIdSpan p i)
  Underscore  -> ensure (idName i == "_")  (setIdSpan p i)
  _           -> error (  "Tried to apply apIdent to Token "
                       ++ show t ++ " on position " ++ show p
                       ++ " which doesn't represent an Ident.")

-- |Add span information to qualified identifier
apQualIdent :: AST.QualIdent -> ASM I.QualIdent
apQualIdent q = getTokenSpan >+=  \ (p, t) -> case t of
  Colon       -> ensure (qidName q == ":")  (setQIdSpan p q)
  Id        i -> ensure (qidName q == i)    (setQIdSpan p q)
  QId       i -> ensure (qidName q == i)    (setQIdSpan p q)
  QSym      i -> ensure (qidName q == i)    (setQIdSpan p q)
  -- no `ensure` here because a symbol like `\\` will be
  -- escaped as `\\\\` and won't be recognized
  Sym       _ -> returnP (setQIdSpan p q)
  SymDot      -> ensure (qidName q == ".")  (setQIdSpan p q)
  SymMinus    -> ensure (qidName q == "-")  (setQIdSpan p q)
  SymMinusDot -> ensure (qidName q == "-.") (setQIdSpan p q)
  Underscore  -> ensure (qidName q == "_")  (setQIdSpan p q)
  _           -> error (  "Tried to apply apQualIdent to Token "
                       ++ show t ++ " on position " ++ show p
                       ++ " which doesn't represent a QualIdent.")

-- |Add span information to qualified identifier but ignore the qualification
apQualIdent2 :: AST.QualIdent -> ASM I.QualIdent
apQualIdent2 q
  = getTokenSpan >+=  \ (p, t) -> case t of
      Id  i -> ensure (qidName2 q == i)    (setQIdSpan p q)
      QId i -> ensure (qidName2 q == i)    (setQIdSpan p q)
      _     -> error (  "Tried to apply apQualIdent2 to Token "
                     ++ show t++ " on position " ++ show p
                     ++ " which doesn't represent a QualIdent.")
  where qidName2 (QualIdent _ i) = idName i

-- |Add span information to identifier that might be surrounded by parens or backticks
apSymIdent :: Ident -> ASM I.SymIdent
apSymIdent i = maybeEnclosedBy
                 [(LeftParen, RightParen), (Backquote, Backquote)]
                 (apIdent i) >+= \ (mpl, i', mpr) ->
               returnP (I.SymIdent mpl i' mpr)

-- |Add span information to qualified identifier that might be surrounded
-- |by parens or backticks
apSymQualIdent :: QualIdent -> ASM I.SymQualIdent
apSymQualIdent qi = maybeEnclosedBy
                      [(LeftParen, RightParen), (Backquote, Backquote)]
                      (apQualIdent qi) >+= \ (mpl, qi', mpr) ->
                    returnP (I.SymQualIdent mpl qi' mpr)

-- |Add span information to module identifier
apModIdent :: AST.ModuleIdent -> ASM I.ModuleIdent
apModIdent mi =
  getTokenSpan >+= \ (p, t) -> case t of
  Id  i -> ensure (moduleName mi == i) (setMIdSpan p mi)
  QId i -> ensure (moduleName mi == i) (setMIdSpan p mi)
  _     -> error (  "Tried to apply apModIdent to Token "
                 ++ show t ++ " on position " ++ show p
                 ++ " which doesn't represent a ModuleIdent.")

-- |Add span information to pattern
apPat :: AST.Pattern -> ASM SpanAST.Pattern
apPat (AST.LiteralPattern l) = SpanAST.LiteralPattern <$> apLit l
apPat (AST.NegativePattern i l) =
  apIdent i >+= \ i' ->
  apLit   l >+= \ l' ->
  returnP (SpanAST.NegativePattern i' l')
apPat (AST.VariablePattern x) = SpanAST.VariablePattern <$> apIdent x
apPat (AST.ConstructorPattern qi pats) =
  apQualIdent qi   >+= \ qi'   ->
  mapM apPat  pats >+= \ pats' ->
  returnP (SpanAST.ConstructorPattern qi' pats')
apPat (AST.InfixPattern pat1 qi pat2) =
  apPat       pat1 >+= \ pat1' ->
  apQualIdent qi   >+= \ qi'   ->
  apPat       pat2 >+= \ pat2' ->
  returnP (SpanAST.InfixPattern pat1' qi' pat2')
apPat (AST.ParenPattern pat) =
  parens (apPat pat)  >+= \ (pl, pat', pr) ->
  returnP (SpanAST.ParenPattern pl pat' pr)
apPat (AST.RecordPattern qi fps) =
  apQualIdent                             qi   >+= \ qi'                   ->
  braces (sepBy apFieldP (tokenSpan Comma) fps) >+= \ (pl, (fps', pss), pr) ->
  returnP (SpanAST.RecordPattern qi' pl fps' pss pr)
apPat (AST.TuplePattern pats) =
  parens (sepBy apPat (tokenSpan Comma) pats) >+= \ (pl, (pats', pss), pr) ->
  returnP (SpanAST.TuplePattern pl pats' pss pr)
apPat (AST.ListPattern pats) =
  brackets (sepBy apPat (tokenSpan Comma) pats) >+= \ (pl, (pats', pss), pr) ->
  returnP (SpanAST.ListPattern pl pats' pss pr)
apPat (AST.AsPattern i pat) =
  apIdent  i   >+= \ i'   ->
  tokenSpan At  >+= \ pa   ->
  apPat    pat >+= \ pat' ->
  returnP (SpanAST.AsPattern i' pa pat')
apPat (AST.LazyPattern pat) =
  tokenSpan Tilde >+= \ pt   ->
  apPat    pat   >+= \ pat' ->
  returnP (SpanAST.LazyPattern pt pat')
apPat (AST.FunctionPattern qi pats) =
  apQualIdent qi   >+= \ qi'   ->
  mapM apPat  pats >+= \ pats' ->
  returnP (SpanAST.FunctionPattern qi' pats')
apPat (AST.InfixFuncPattern pat1 qi pat2) =
  apPat       pat1 >+= \ pat1' ->
  apQualIdent qi   >+= \ qi'   ->
  apPat       pat2 >+= \ pat2' ->
  returnP (SpanAST.InfixFuncPattern pat1' qi' pat2')

-- |Add span information to expression
apExpr :: AST.Expression -> ASM SpanAST.Expression
apExpr (AST.Literal     l) = SpanAST.Literal     <$> apLit       l
apExpr (AST.Variable    x) = SpanAST.Variable    <$> apSymQualIdent x
apExpr (AST.Constructor c) = SpanAST.Constructor <$> apSymQualIdent c
apExpr (AST.Paren       e) =
  parens (apExpr   e) >+= \ (pl, e', pr) ->
  returnP (SpanAST.Paren pl e' pr)
apExpr (AST.Typed e te) =
  apExpr     e           >+= \ e'  ->
  tokenSpan   DoubleColon >+= \ p   ->
  apTypeExpr te          >+= \ te' ->
  returnP (SpanAST.Typed e' p te')
apExpr (AST.Record qi fes) =
  apQualIdent                             qi   >+= \ qi'                   ->
  braces (sepBy apFieldE (tokenSpan Comma) fes) >+= \ (pl, (pfes, pss), pr) ->
  returnP (SpanAST.Record qi' pl pfes pss pr)
apExpr (AST.RecordUpdate e fes) =
  apExpr                                  e    >+= \ e'                    ->
  braces (sepBy apFieldE (tokenSpan Comma) fes) >+= \ (pl, (fes', pss), pr) ->
  returnP (SpanAST.RecordUpdate e' pl fes' pss pr)
apExpr (AST.Tuple es) =
  parens (sepBy apExpr (tokenSpan Comma) es) >+=
    \ (pl, (pes, pss), pr) ->
  returnP (SpanAST.Tuple pl pes pss pr)
apExpr (AST.List es) =
  brackets (sepBy apExpr (tokenSpan Comma) es) >+=
    \ (pl, (pes, pss), pr) ->
  returnP (SpanAST.List pl pes pss pr)
apExpr (AST.ListCompr e sts) =
    brackets (apListCompr e sts) >+= \ (pl, (e', pb, psts, pss), pr) ->
    returnP (SpanAST.ListCompr pl e' pb psts pss pr)
  where
    apListCompr e_ sts_ =
      apExpr                        e_     >+= \ e'          ->
      tokenSpan                      Bar    >+= \ pb          ->
      sepBy apStmt (tokenSpan Comma) sts_   >+= \ (psts, pss) ->
      returnP (e', pb, psts, pss)
apExpr (AST.EnumFrom e) =
    brackets (apEnumFrom e) >+= \ (pl, (e', pd), pr) ->
    returnP (SpanAST.EnumFrom pl e' pd pr)
  where
    apEnumFrom e_ =
      apExpr   e_      >+= \ e' ->
      tokenSpan DotDot  >+= \ pd ->
      returnP (e', pd)
apExpr (AST.EnumFromThen e1 e2) =
    brackets (apEnumFromThen e1 e2) >+= \ (pl, (e1', pc, e2', pd), pr) ->
    returnP (SpanAST.EnumFromThen pl e1' pc e2' pd pr)
  where
    apEnumFromThen e1_ e2_ =
      apExpr   e1_           >+= \ e1' ->
      tokenSpan Comma         >+= \ pc  ->
      apExpr   e2_           >+= \ e2' ->
      tokenSpan DotDot        >+= \ pd  ->
      returnP (e1', pc, e2', pd)
apExpr (AST.EnumFromTo e1 e2) =
    brackets (apEnumFromTo e1 e2) >+= \ (pl, (e1', pd, e2'), pr) ->
    returnP (SpanAST.EnumFromTo pl e1' pd e2' pr)
  where
    apEnumFromTo e1_ e2_ =
      apExpr   e1_           >+= \ e1' ->
      tokenSpan DotDot        >+= \ pd  ->
      apExpr   e2_           >+= \ e2' ->
      returnP (e1', pd, e2')
apExpr (AST.EnumFromThenTo e1 e2 e3) =
    brackets (apEnumFromThenTo e1 e2 e3) >+=
      \ (pl, (e1', pc, e2', pd, e3'), pr) ->
    returnP (SpanAST.EnumFromThenTo pl e1' pc e2' pd e3' pr)
  where
    apEnumFromThenTo e1_ e2_ e3_ =
      apExpr   e1_           >+= \ e1' ->
      tokenSpan Comma         >+= \ pc  ->
      apExpr   e2_           >+= \ e2' ->
      tokenSpan DotDot        >+= \ pd  ->
      apExpr   e3_           >+= \ e3' ->
      returnP (e1', pc, e2', pd, e3')
apExpr (AST.UnaryMinus i e) =
  apIdent i >+= \ i' ->
  apExpr  e >+= \ e' ->
  returnP (SpanAST.UnaryMinus i' e')
apExpr (AST.Apply e1 e2) =
  apExpr e1 >+= \ e1' ->
  apExpr e2 >+= \ e2' ->
  returnP (SpanAST.Apply e1' e2')
apExpr (AST.InfixApply e1 iop e2) =
  apExpr    e1  >+= \ e1'  ->
  apInfixOp iop >+= \ iop' ->
  apExpr    e2  >+= \ e2'  ->
  returnP (SpanAST.InfixApply e1' iop' e2')
apExpr (AST.LeftSection e iop) =
    parens (apLeftSection e iop) >+= \ (pl, (e', iop'), pr) ->
    returnP (SpanAST.LeftSection pl e' iop' pr)
  where
    apLeftSection e_ iop_ =
      apExpr    e_          >+= \ e'   ->
      apInfixOp iop_        >+= \ iop' ->
      returnP (e', iop')
apExpr (AST.RightSection iop e) =
    parens (apRightSection iop e) >+= \ (pl, (iop', e'), pr) ->
    returnP (SpanAST.RightSection pl iop' e' pr)
  where
    apRightSection iop_ e_ =
      apInfixOp iop_        >+= \ iop' ->
      apExpr    e_          >+= \ e'   ->
      returnP (iop', e')
apExpr (AST.Lambda pats e) =
  tokenSpan   Backslash  >+= \ pl    ->
  mapM apPat pats       >+= \ pats' ->
  tokenSpan   RightArrow >+= \ pa    ->
  apExpr     e          >+= \ e'    ->
  returnP (SpanAST.Lambda pl pats' pa e')
apExpr (AST.Let decls e) =
  tokenSpan    KW_let >+= \ pl     ->
  mapM apDecl decls  >+= \ decls' ->
  tokenSpan    KW_in  >+= \ pi     ->
  apExpr      e      >+= \ e'     ->
  returnP (SpanAST.Let pl decls' pi e')
apExpr (AST.Do stmts e) =
  tokenSpan    KW_do >+= \ pd     ->
  mapM apStmt stmts >+= \ stmts' ->
  apExpr      e     >+= \ e'     ->
  returnP (SpanAST.Do pd stmts' e')
apExpr (AST.IfThenElse i t e) =
  tokenSpan KW_if    >+= \ pi ->
  apExpr   i        >+= \ i' ->
  tokenSpan KW_then  >+= \ pt ->
  apExpr   t        >+= \ t' ->
  tokenSpan KW_else  >+= \ pe ->
  apExpr   e        >+= \ e' ->
  returnP (SpanAST.IfThenElse pi i' pt t' pe e')
apExpr (AST.Case ct e alts) =
  apCaseType ct      >+= \ ct'   ->
  tokenSpan   KW_case >+= \ pc    ->
  apExpr     e       >+= \ e'    ->
  tokenSpan   KW_of   >+= \ po    ->
  mapM apAlt alts    >+= \ alts' ->
  returnP (SpanAST.Case ct' pc e' po alts')

-- |Add span information to InfixOp
apInfixOp :: AST.InfixOp -> ASM SpanAST.InfixOp
apInfixOp (AST.InfixOp qi) =
  apSymQualIdent qi >+= \ qis' ->
  returnP (SpanAST.InfixOp qis')
apInfixOp (AST.InfixConstr qi) =
  apSymQualIdent qi >+= \ qis' ->
  returnP (SpanAST.InfixConstr qis')

-- |Add span information to statement
apStmt :: AST.Statement -> ASM SpanAST.Statement
apStmt (AST.StmtExpr e) =
  apExpr e >+= \ e' ->
  returnP (SpanAST.StmtExpr e')
apStmt (AST.StmtDecl ds) =
  tokenSpan    KW_let >+= \ pl  ->
  mapM apDecl ds     >+= \ ds' ->
  returnP (SpanAST.StmtDecl pl ds')
apStmt (AST.StmtBind pat e) =
  apPat    pat       >+= \ pat' ->
  tokenSpan LeftArrow >+= \ pla  ->
  apExpr   e         >+= \ e'   ->
  returnP (SpanAST.StmtBind pla pat' e')

-- |Convert casetype
apCaseType :: AST.CaseType -> ASM SpanAST.CaseType
apCaseType (AST.Rigid) = returnP (SpanAST.Rigid)
apCaseType (AST.Flex)  = returnP (SpanAST.Flex)

-- |Add span information to case-alternative
apAlt :: AST.Alt -> ASM SpanAST.Alt
apAlt (AST.Alt _ pat rhs) =
  apPat    pat                  >+= \ pat' ->
  apRhs    rhs RightArrow       >+= \ rhs' ->
  returnP (SpanAST.Alt pat' rhs')

-- |Add span information to a field of expressions
apFieldE :: AST.Field AST.Expression -> ASM (SpanAST.Field SpanAST.Expression)
apFieldE (AST.Field _ qi e) =
  apQualIdent qi     >+= \ qi' ->
  tokenSpan    Equals >+= \ pe  ->
  apExpr      e      >+= \ e'  ->
  returnP (SpanAST.Field qi' pe e')

-- |Add span information to a field of patterns
apFieldP :: AST.Field AST.Pattern -> ASM (SpanAST.Field SpanAST.Pattern)
apFieldP (AST.Field _ qi pat) =
  apQualIdent qi     >+= \ qi'  ->
  tokenSpan    Equals >+= \ pe   ->
  apPat       pat    >+= \ pat' ->
  returnP (SpanAST.Field qi' pe pat')
