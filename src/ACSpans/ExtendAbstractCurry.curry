-- This module extends the AbstractCurry representation of a program
-- with span information provided by a span-annotated AST.
--
-- Since there are some differences regarding the representation of a program
-- in AbstractCurry and its AST, we extend the type, function and operator
-- declarations of an AbstractCurry program successively.
-- For this purpose, we put all AST declarations in different kinds of
-- finite maps. Thus, we can lookup the corresponding AST declaration when
-- we extend a specific AbstractCurry declaration.

module ACSpans.ExtendAbstractCurry where

import qualified AbstractCurry.Types as AC
import AbstractCurry.Select (funcName)
import FiniteMap
import Function (on)
import List (groupBy, init, last, sortBy)

import qualified ACSpans.AbstractCurrySpan as ACS
import ACSpans.Ident
import ACSpans.Span (Span, virtualSpan)
import ACSpans.SpanAST
import ACSpans.SpanUtils

eProg :: AC.CurryProg -> Module -> ACS.CurryProg
eProg (AC.CurryProg m is ts fs os) mod@(Module _ _ _ _ _ _ ds)
  = let types = foldr bindType (emptyFM (<)) (filter isTypeDecl ds)
        funs  = foldr bindSig
                (foldr bindFun (emptyFM (<))
              $ filter isExternalDecl ds ++ joinFunDecls (filter isFuncDecl ds))
              $ filter isTypeSig ds
        iops  = foldr bindOp (emptyFM (<)) (filter isInfixDecl ds)
        ts'   = map (eTypeDecl types) ts
        fs'   = map (eFuncDeclFM funs) fs
        os'   = map (eOpDecl iops) os
    in ACS.CurryProg (spanModule mod) m is ts' fs' os'

-- -----------------------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------------------

eTypeDecl :: FM String Decl -> AC.CTypeDecl -> ACS.CTypeDecl
eTypeDecl types (AC.CType qn@(_,i) vis vs cs) = case lookupFM types i of
  Nothing -> error $ "ExtendAbstractCurry.eTypeDecl: Type " ++ i ++ " not found"
  Just d  -> case d of
    DataDecl _ n vars _ cons _ -> let qn' = eQNameIdent qn n
                                      vs' = zipWith eTVarIdent vs vars
                                      cs' = zipWith eConsDecl cs cons
                                  in ACS.CType (spanDecl d) qn' (convVis vis) vs' cs'
    _                          -> error "ExtendAbstractCurry.eTypeDecl: no type declaration"
eTypeDecl types (AC.CTypeSyn qn@(_,i) vis vs ty) = case lookupFM types i of
  Nothing -> error $ "ExtendAbstractCurry.eTypeDecl: Type " ++ i ++ " not found"
  Just d -> case d of
    TypeDecl _ n vars _ te -> let qn' = eQNameIdent qn n
                                  vs' = zipWith eTVarIdent vs vars
                                  ty' = addVSpans ty
                              in ACS.CTypeSyn (spanDecl d) qn' (convVis vis) vs' ty'
    _                      -> error "ExtendAbstractCurry.eTypeDecl: no type declaration"
eTypeDecl types (AC.CNewType qn@(_,i) vis vs c) = case lookupFM types i of
  Nothing -> error $ "ExtendAbstractCurry.eTypeDecl: Type " ++ i ++ " not found"
  Just d  -> case d of
    NewtypeDecl _ n vars _ con -> let qn' = eQNameIdent qn n
                                      vs' = zipWith eTVarIdent vs vars
                                      c'  = eNewConsDecl c con
                                  in ACS.CNewType (spanDecl d) qn' (convVis vis) vs' c'
    _                          -> error "ExtendAbstractCurry.eTypeDecl: no type declaration"

eConsDecl :: AC.CConsDecl -> ConstrDecl -> ACS.CConsDecl
eConsDecl c c' = case (c, c') of
  (AC.CCons qn vis tys , ConstrDecl  _ i tys'    ) ->
    ACS.CCons (spanConstr c') (eQNameIdent qn i) (convVis vis) (map addVSpans tys)
  (AC.CCons qn vis tys , ConOpDecl _ ty1' i ty2' ) ->
    ACS.CCons (spanConstr c') (eQNameIdent qn i) (convVis vis) (map addVSpans tys)
  (AC.CRecord qn vis fs, RecordDecl _ i _ fs' _ _) ->
    ACS.CRecord (spanConstr c') (eQNameIdent qn i) (convVis vis) (zipWith eField fs fs')
  _                                                ->
    error $ "ExtendAbstractCurry.eConsDecl: no rule for " ++ show c ++ " and " ++ show c'

eNewConsDecl :: AC.CConsDecl -> NewConstrDecl -> ACS.CConsDecl
eNewConsDecl c nc = case (c, nc) of
  (AC.CCons qn vis tys , NewConstrDecl _ i ty'             ) ->
    ACS.CCons (spanNewConstr nc) (eQNameIdent qn i) (convVis vis) (map addVSpans tys)
  (AC.CRecord qn vis fs, NewRecordDecl _ qi _ (f',s',ty') _) ->
    let fs'  = zipWith eField fs [FieldDecl [f'] [] s' ty']
    in ACS.CRecord (spanNewConstr nc) (eQNameIdent qn qi) (convVis vis) fs'
  _                                                          ->
    error $ "ExtendAbstractCurry.eNewConsDecl: no rule for " ++ show c ++ " and " ++ show nc

eField :: AC.CFieldDecl -> FieldDecl -> ACS.CFieldDecl
eField f f' = case (f, f') of
  (AC.CField qn vis ty, FieldDecl [i] _ _ ty') ->
    ACS.CField (spanField f') (eQNameIdent qn i) (convVis vis) (addVSpans ty)
  _                                            ->
    error $ "ExtendAbstractCurry.eField: no rule for " ++ show f ++ " and " ++ show f'

-- -----------------------------------------------------------------------------
-- Type expressions
-- -----------------------------------------------------------------------------

-- Since Curry allows type expressions in parentheses which are only represented
-- in the AST but not in AbstractCurry, we update the span information
-- computed for an AbstractCurry type expressions with `eTypeExpr` afterwards
-- if necessary (i.e. if the type expression was parenthesized in the original
-- program)
--
-- Additionally, AST type expressions are desugared before their span
-- information is used to extend the AbstractCurry representation.
-- This is necessary, because some type expressions like tuple and list types
-- have only an explicit representation in the AST.

-- | Update span information of an AbstractCurry type expression
setSpanTypeExpr :: Span -> ACS.CTypeExpr -> ACS.CTypeExpr
setSpanTypeExpr s (ACS.CTVar          _ tv) = ACS.CTVar s tv
setSpanTypeExpr s (ACS.CFuncType _ ty1 ty2) = ACS.CFuncType s ty1 ty2
setSpanTypeExpr s (ACS.CTCons     _ qn tys) = ACS.CTCons s qn tys

-- | Desugar AST type expressions and extend corresponding AbstractCurry
-- representation with span information
eTypeExpr :: AC.CTypeExpr -> TypeExpr -> ACS.CTypeExpr
eTypeExpr ty ty' = case (ty, ty') of
  (_                   , ParenType _ ty1' _        ) ->
    setSpanTypeExpr (spanTypeExpr ty') (eTypeExpr ty ty1')
  (AC.CTVar tv         , VariableType i            ) ->
    ACS.CTVar (spanTypeExpr ty') (eTVarIdent tv i)
  (AC.CFuncType ty1 ty2, ArrowType ty1' _ ty2'     ) ->
    ACS.CFuncType (spanTypeExpr ty') (eTypeExpr ty1 ty1') (eTypeExpr ty2 ty2')
  (AC.CTCons qn tys    , ConstructorType _ qi tes _) ->
    ACS.CTCons (spanTypeExpr ty') (eQNameQIdent qn qi) (zipWith eTypeExpr tys tes)
  (AC.CTCons _ _       , TupleType _ tys' _ _      ) ->
    let qid = if null tys' then qUnitId else qTupleId (length tys')
    in eTypeExpr ty (ConstructorType Nothing qid tys' Nothing)
  (AC.CTCons _ _       , ListType _ ty1' _         ) ->
    eTypeExpr ty (ConstructorType Nothing qListId [ty1'] Nothing)
  _                                                  ->
    error $ "ExtendAbstractCurry.eTypeExpr: no rule for " ++ show ty ++ " and " ++ show ty'

-- -----------------------------------------------------------------------------
-- Functions
-- -----------------------------------------------------------------------------

-- We extend function declarations with span information. We also consider
-- type signatures: If the type of a function was specified in the original
-- program, we add the corresponding span information included in the AST.
-- Otherwise, the type signatures are extended with virtual span information
-- (i.e. spans of the form ((0,0),(0,0)).

eFuncDeclFM :: FM String (Decl, Maybe TypeExpr) -> AC.CFuncDecl -> ACS.CFuncDecl
eFuncDeclFM funs f = case lookupFM funs fname of
  Nothing       -> error $ "ExtendAbstractCurry.eFuncDeclFM: Function " ++ fname ++ " not found"
  Just (d, mty) -> eFuncDecl f d mty
 where
  fname = snd (funcName f)

eFuncDecl :: AC.CFuncDecl -> Decl -> Maybe TypeExpr -> ACS.CFuncDecl
eFuncDecl f f' mty = case (f, f') of
  (AC.CFunc qn a vis ty rs      , FunctionDecl i eqs  ) ->
    let mspan = if adjacent rs' then Just (spanDecl f') else Nothing
        qn'   = eQNameIdent qn i
        ty'   = addVSpans ty
        rs'   = zipWith eRule rs eqs
    in ACS.CFunc mspan qn' a (convVis vis) ty' rs'
  (AC.CFunc qn a vis ty _       , ExternalDecl [i] _ _) ->
    let mspan = Just (spanDecl f')
        qn'   = eQNameSIdent qn i
        ty'   = addVSpans ty
    in ACS.CFunc mspan qn' a (convVis vis) ty' []
  (AC.CmtFunc str qn a vis ty rs, FunctionDecl i eqs  ) ->
    let mspan = if adjacent rs' then Just (spanDecl f') else Nothing
        qn'   = eQNameIdent qn i
        ty'   = addVSpans ty
        rs'   = zipWith eRule rs eqs
    in ACS.CmtFunc mspan str qn' a (convVis vis) ty' rs'
  (AC.CmtFunc str qn a vis ty _ , ExternalDecl [i] _ _) ->
    let mspan = Just (spanDecl f')
        qn'   = eQNameSIdent qn i
        ty'   = addVSpans ty
    in ACS.CmtFunc mspan str qn' a (convVis vis) ty' []
  _                                                     ->
    error $ "ExtendAbstractCurry.eFuncDecl: no rule for " ++ show f ++ " and " ++ show f'

eRule :: AC.CRule -> Equation -> ACS.CRule
eRule (AC.CRule ps rs) eq@(Equation lhs rhs)
  = let ps' = zipWith ePat ps (lhsPattern lhs)
        rs' = eRhs rs rhs
    in ACS.CRule (spanEquation eq) ps' rs'

-- -----------------------------------------------------------------------------
-- Pattern
-- -----------------------------------------------------------------------------

-- As Curry allows the specification of parenthesized patterns which are not
-- represented in AbstractCurry, we possibly have to update the span information
-- computed for a pattern with the span information of the parenthesized
-- pattern.
--
-- Furthermore some patterns are desugared before adding span information,
-- because infix, tuple and list pattern are only represented explicitly in
-- the AST representation but not in AbstractCurry.

-- | Update the span information of a pattern
setSpanPat :: Span -> ACS.CPattern -> ACS.CPattern
setSpanPat s (ACS.CPVar         _ vn) = ACS.CPVar s vn
setSpanPat s (ACS.CPLit          _ l) = ACS.CPLit s l
setSpanPat s (ACS.CPComb     _ qn ps) = ACS.CPComb s qn ps
setSpanPat s (ACS.CPAs        _ vn p) = ACS.CPAs s vn p
setSpanPat s (ACS.CPFuncComb _ qn ps) = ACS.CPFuncComb s qn ps
setSpanPat s (ACS.CPLazy         _ p) = ACS.CPLazy s p
setSpanPat s (ACS.CPRecord   _ qn fs) = ACS.CPRecord s qn fs

-- | Desugar AST patterns and extend their corresponding AbstractCurry
-- representation with span information
ePat :: AC.CPattern -> Pattern -> ACS.CPattern
ePat p p' = case (p, p') of
  (_                  , ParenPattern _ pat _       ) ->
    setSpanPat (spanPat p') (ePat p pat)
  (AC.CPVar vn        , VariablePattern i          ) ->
    ACS.CPVar (spanPat p') (eVarIdent vn i)
  (AC.CPLit l         , LiteralPattern l'          ) ->
    ACS.CPLit (spanPat p') (eLiteral l l')
  (AC.CPLit _         , NegativePattern _ l'       ) ->
    ePat p (LiteralPattern (negateLit l'))
  (AC.CPComb qn ps    , ConstructorPattern qi ps'  ) ->
    ACS.CPComb (spanPat p') (eQNameQIdent qn qi) (zipWith ePat ps ps')
  (AC.CPComb _ _      , InfixPattern p1' op p2'    ) ->
    ePat p (ConstructorPattern op [p1', p2'])
  (AC.CPComb _ _      , TuplePattern _ ps' _ _     ) ->
    let pat = case ps' of
                 [] -> ConstructorPattern qUnitId []
                 _  -> ConstructorPattern (qTupleId $ length ps') ps'
    in ePat p pat
  (AC.CPComb _ _      , ListPattern _ ps' _ _      ) ->
    let pat = foldr (\x1 x2 -> ConstructorPattern qConsId [x1, x2])
                     (ConstructorPattern qNilId [])
                     ps'
    in ePat p pat
  (AC.CPAs vn p1      , AsPattern i _ p1'          ) ->
    ACS.CPAs (spanPat p') (eVarIdent vn i) (ePat p1 p1')
  (AC.CPFuncComb qn ps, FunctionPattern qi ps'     ) ->
    ACS.CPFuncComb (spanPat p') (eQNameQIdent qn qi) (zipWith ePat ps ps')
  (AC.CPFuncComb _ _  , InfixFuncPattern p1' qi p2') ->
    ePat p (FunctionPattern qi [p1', p2'])
  (AC.CPLazy p1       , LazyPattern _ p1'          ) ->
    ACS.CPLazy (spanPat p') (ePat p1 p1')
  (AC.CPRecord qn fs  , RecordPattern qi _ fs' _ _ ) ->
    ACS.CPRecord (spanPat p') (eQNameQIdent qn qi) (zipWith (eFieldWith ePat) fs fs')
  _                                                  ->
    error $ "ExtendAbstractCurry.ePat: no rule for " ++ show p ++ " and " ++ show p'


eRhs :: AC.CRhs -> Rhs -> ACS.CRhs
eRhs rhs rhs' = case (rhs, rhs') of
  (AC.CSimpleRhs e ds   , SimpleRhs _ e' _ ds'     ) ->
    ACS.CSimpleRhs (spanRhs rhs') (eExpr e e') (zipWith eLocal ds (flattenLocal ds'))
  (AC.CGuardedRhs ces ds, GuardedRhs _ ces' _ _ ds') ->
    ACS.CGuardedRhs (spanRhs rhs') (zipWith eCondExpr ces ces')
                                   (zipWith eLocal ds (flattenLocal ds'))
  _                                                  ->
    error $ "ExtendAbstractCurry.eRhs: no rule for " ++ show rhs ++ " and " ++ show rhs'

eCondExpr :: (AC.CExpr, AC.CExpr) -> CondExpr -> (Span, ACS.CExpr, ACS.CExpr)
eCondExpr (c, e) cexp@(CondExpr cond _ exp)
  = (spanCondExpr cexp, eExpr c cond, eExpr e exp)

-- -----------------------------------------------------------------------------
-- Expressions
-- -----------------------------------------------------------------------------

-- Curry allows the specification of parenthesized expressions which are not
-- explicitly represented in AbstractCurry. For that reason, we update the span
-- information computed for an expression if necessary.
--
-- Furthermore as for type expressions and patterns some kinds of expressions
-- are desugared before adding span information, because they do not have a
-- corresponding AbstractCurry representation (e.g. tuples and lists).

-- | Update the span information of an expression
setSpanExpr :: Span -> ACS.CExpr -> ACS.CExpr
setSpanExpr s (ACS.CVar          _ vn) = ACS.CVar s vn
setSpanExpr s (ACS.CLit           _ l) = ACS.CLit s l
setSpanExpr s (ACS.CSymbol       _ qn) = ACS.CSymbol s qn
setSpanExpr s (ACS.CApply     _ e1 e2) = ACS.CApply s e1 e2
setSpanExpr s (ACS.CLambda     _ ps e) = ACS.CLambda s ps e
setSpanExpr s (ACS.CLetDecl    _ ds e) = ACS.CLetDecl s ds e
setSpanExpr s (ACS.CDoExpr       _ ss) = ACS.CDoExpr s ss
setSpanExpr s (ACS.CListComp   _ e ss) = ACS.CListComp s e ss
setSpanExpr s (ACS.CCase    _ ct e bs) = ACS.CCase s ct e bs
setSpanExpr s (ACS.CTyped      _ e ty) = ACS.CTyped s e ty
setSpanExpr s (ACS.CRecConstr _ qn fs) = ACS.CRecConstr s qn fs
setSpanExpr s (ACS.CRecUpdate  _ e fs) = ACS.CRecUpdate s e fs

-- | Desugar AST expressions and extend their corresponding AbstractCurry
-- representation with span information
eExpr :: AC.CExpr -> Expression -> ACS.CExpr
eExpr e e' = case (e,e') of
  (_                  , Paren                      _ exp _) ->
    setSpanExpr (spanExpr e') (eExpr e exp)
  (AC.CVar vn         , Variable                       sqi) ->
    ACS.CVar (spanExpr e') (eVarSQIdent vn sqi)
  (AC.CLit l          , Literal                         l') ->
    ACS.CLit (spanExpr e') (eLiteral l l')
  (AC.CSymbol qn      , Variable                       sqi) ->
    ACS.CSymbol (spanExpr e') (eQNameSQIdent qn sqi)
  (AC.CSymbol qn      , Constructor                    sqi) ->
    ACS.CSymbol (spanExpr e') (eQNameSQIdent qn sqi)
  (AC.CSymbol _       , Tuple                     _ [] _ _) ->
    eExpr e (Variable $ mkSQIdent qUnitId)
  (AC.CSymbol _       , List                      _ [] _ _) ->
    eExpr e (Constructor $ mkSQIdent qNilId)
  (AC.CApply e1 e2    , Apply                      e1' e2') ->
    ACS.CApply (spanExpr e') (eExpr e1 e1') (eExpr e2 e2')
  (AC.CApply _ _      , InfixApply              e1' op e2') ->
    eExpr e (apply (opToExpr op) [e1', e2'])
  (AC.CApply _ _      , Tuple                     _ es _ _) ->
    eExpr e (apply (Variable $ mkSQIdent $ qTupleId $ length es) es)
  (AC.CApply _ _      , List                      _ es _ _) ->
    eExpr e (foldr (Apply . Apply (Constructor $ mkSQIdent qConsId)) (Constructor $ mkSQIdent qNilId) es)
  (AC.CApply _ _      , EnumFrom                 _ e1' _ _) ->
    eExpr e (apply (Variable $ mkSQIdent $ qEnumFromId) [e1'])
  (AC.CApply _ _      , EnumFromThen       _ e1' _ e2' _ _) ->
    eExpr e (apply (Variable $ mkSQIdent $ qEnumFromThenId) [e1', e2'])
  (AC.CApply _ _      , EnumFromTo           _ e1' _ e2' _) ->
    eExpr e (apply (Variable $ mkSQIdent $ qEnumFromToId) [e1', e2'])
  (AC.CApply _ _      , EnumFromThenTo _ e1' _ e2' _ e3' _) ->
    eExpr e (apply (Variable $ mkSQIdent $ qEnumFromThenToId) [e1', e2', e3'])
  (AC.CApply _ _      , UnaryMinus                   _ e1') ->
    eExpr e (apply (Variable $ mkSQIdent $ qNegateId) [e1'])
  (AC.CApply _ _      , LeftSection             _ e1' op _) ->
    eExpr e (apply (opToExpr op) [e1'])
  (AC.CApply _ _      , RightSection            _ op e1' _) ->
    eExpr e (apply (Variable $ mkSQIdent $ qFlip) [opToExpr op, e1'])
  (AC.CApply _ _      , IfThenElse       _ e1' _ e2' _ e3') ->
    eExpr e (apply (Variable $ mkSQIdent $ qIfThenElseId) [e1', e2', e3'])
  (AC.CLambda ps e1   , Lambda                 _ ps' _ e1') ->
    ACS.CLambda (spanExpr e') (zipWith ePat ps ps') (eExpr e1 e1')
  (AC.CLetDecl ds e1  , Let                    _ ds' _ e1') ->
    ACS.CLetDecl (spanExpr e') (zipWith eLocal ds (flattenLocal ds')) (eExpr e1 e1')
  (AC.CDoExpr ss      , Do                       _ ss' e1') ->
    ACS.CDoExpr (spanExpr e') (eStatements ss ss' e1')
  (AC.CListComp e1 ss , ListCompr          _ e1' _ ss' _ _) ->
    ACS.CListComp (spanExpr e') (eExpr e1 e1') (zipWith eStatement ss ss')
  (AC.CCase ct e1 bs  , Case                 _ _ e1' _ bs') ->
    ACS.CCase (spanExpr e') (convCas ct) (eExpr e1 e1') (zipWith eBranch bs bs')
  (AC.CTyped e1 ty    , Typed                    e1' _ ty') ->
    ACS.CTyped (spanExpr e') (eExpr e1 e1') (addVSpans ty)
  (AC.CRecConstr qn fs, Record                qi _ fs' _ _) ->
    ACS.CRecConstr (spanExpr e') (eQNameQIdent qn qi) (zipWith (eFieldWith eExpr) fs fs')
  (AC.CRecUpdate e1 fs, RecordUpdate         e1' _ fs' _ _) ->
    ACS.CRecUpdate (spanExpr e') (eExpr e1 e1') (zipWith (eFieldWith eExpr) fs fs')
  _                                                         ->
    error $ "ExtendAbstractCurry.eExpr: no rule for " ++ show e ++ " and " ++ show e'

eLiteral :: AC.CLiteral -> Literal -> ACS.CLiteral
eLiteral (AC.CIntc    i) lit = ACS.CIntc    (spanLiteral lit) i
eLiteral (AC.CFloatc  f) lit = ACS.CFloatc  (spanLiteral lit) f
eLiteral (AC.CCharc   c) lit = ACS.CCharc   (spanLiteral lit) c
eLiteral (AC.CStringc s) lit = ACS.CStringc (spanLiteral lit) s

eLocal :: AC.CLocalDecl -> Decl -> ACS.CLocalDecl
eLocal d d' = case (d, d') of
  (AC.CLocalFunc f   , _                  ) ->
    ACS.CLocalFunc (spanDecl d') (eFuncDecl f d' Nothing)
  (AC.CLocalPat p rhs, PatternDecl p' rhs') ->
    ACS.CLocalPat (spanDecl d') (ePat p p') (eRhs rhs rhs')
  (AC.CLocalVars vs  , FreeDecl is _ _    ) ->
    ACS.CLocalVars (spanDecl d') (zipWith eVarIdent vs is)
  _                                         ->
    error $ "ExtendAbstractCurry.eLocal: no rule for " ++ show d ++ " and " ++ show d'

eStatements :: [AC.CStatement] -> [Statement] -> Expression -> [ACS.CStatement]
eStatements ss stms exp = case ss of
  [AC.CSExpr e] -> [ACS.CSExpr (spanExpr exp) (eExpr e exp)]
  _             -> case last ss of
                     AC.CSExpr e1 -> let e'  = eExpr e1 exp
                                         ss' = zipWith eStatement (init ss) stms
                                     in ss' ++ [ACS.CSExpr (spanExpr exp) e']
                     _            -> error $ "eStatements: invalid statement"

eStatement :: AC.CStatement -> Statement -> ACS.CStatement
eStatement s s' = case (s, s') of
  (AC.CSExpr e , StmtExpr e'     ) -> ACS.CSExpr (spanStmt s') (eExpr e e')
  (AC.CSPat p e, StmtBind _ p' e') ->
    ACS.CSPat (spanStmt s') (ePat p p') (eExpr e e')
  (AC.CSLet ds , StmtDecl _ ds'  ) ->
    ACS.CSLet (spanStmt s') (zipWith eLocal ds (flattenLocal ds'))
  _                                ->
    error $ "ExtendAbstractCurry.eStatement: no rule for " ++ show s ++ " and " ++ show s'

eBranch :: (AC.CPattern, AC.CRhs) -> Alt -> (ACS.CPattern, ACS.CRhs)
eBranch (p, rs) (Alt pat rhs) = (ePat p pat, eRhs rs rhs)

-- | Extend a field pattern / a field expression with span information
-- using the given function `extend`
eFieldWith :: (a -> b -> c) -> AC.CField a -> Field b -> ACS.CField c
eFieldWith extend (qn, x) (Field qi _ y) = (eQNameQIdent qn qi, extend x y)

eOpDecl :: FM String Decl -> AC.COpDecl -> ACS.COpDecl
eOpDecl iops (AC.COp qn@(_,n) fix prec) = case lookupFM iops n of
  Nothing -> error $ "ExtendAbstractCurry.eOpDecl: Operator " ++ n ++ " not found"
  Just d  -> case d of
    InfixDecl _ _ [op] _ -> ACS.COp (spanDecl d) (eQNameIdent qn (opIdent op)) (convFix fix) prec
    _                    -> error "ExtendAbstractCurry.eOpDecl: no operator declaration"

-- -----------------------------------------------------------------------------
-- Identifiers
-- -----------------------------------------------------------------------------

-- Extend qualified names, variables and types variables with span information

eQNameIdent :: AC.QName -> Ident -> ACS.QName
eQNameIdent (m, n) i = (spanIdent i, m, n)

eQNameSIdent :: AC.QName -> SymIdent -> ACS.QName
eQNameSIdent (m, n) si = (spanSIdent si, m, n)

eQNameQIdent :: AC.QName -> QualIdent -> ACS.QName
eQNameQIdent (m, n) qi = (spanQIdent qi, m, n)

eQNameSQIdent :: AC.QName -> SymQualIdent -> ACS.QName
eQNameSQIdent (m, n) sqi = (spanSQIdent sqi, m, n)

eVarIdent :: AC.CVarIName -> Ident -> ACS.CVarIName
eVarIdent (idx, n) i = (spanIdent i, idx, n)

eVarSQIdent :: AC.CVarIName -> SymQualIdent -> ACS.CVarIName
eVarSQIdent (idx, n) sqi = (spanSQIdent sqi, idx, n)

eTVarIdent :: AC.CTVarIName -> Ident -> ACS.CTVarIName
eTVarIdent (idx, n) i = (spanIdent i, idx, n)

-- -----------------------------------------------------------------------------
-- Utility functions
-- -----------------------------------------------------------------------------

-- | Is the declaration a function declaration?
isFuncDecl :: Decl -> Bool
isFuncDecl d = case d of
  FunctionDecl _ _ -> True
  _                -> False

-- | Is the declaration an external declaration?
isExternalDecl :: Decl -> Bool
isExternalDecl d = case d of
  ExternalDecl _ _ _ -> True
  _                  -> False

-- | Is the declaration a type declaration?
isTypeDecl :: Decl -> Bool
isTypeDecl d = case d of
  DataDecl  _ _ _ _ _ _ -> True
  NewtypeDecl _ _ _ _ _ -> True
  TypeDecl    _ _ _ _ _ -> True
  _                     -> False

-- | Is the declaration an infix operator declaration?
isInfixDecl :: Decl -> Bool
isInfixDecl d = case d of
  InfixDecl _ _ _ _ -> True
  _                 -> False

-- | Is the declaration a type signature?
isTypeSig :: Decl -> Bool
isTypeSig d = case d of
  TypeSig         _ _ _ _ -> True
  ForeignDecl _ _ _ _ _ _ -> True
  _                       -> False

-- | Get the identifier of a function declaration
fName :: Decl -> Ident
fName d = case d of
  FunctionDecl f _ -> f
  _                -> error "ExtendAbstractCurry.fName: no function declaration"

-- | Get equations of a function declaration
fEqs :: Decl -> [Equation]
fEqs d = case d of
 FunctionDecl _ eqs -> eqs
 _                  -> error "ExtendAbstractCurry.fEqs: no function declaration"

-- | Get the identifier of an 'SymIdent'
opIdent :: SymIdent -> Ident
opIdent (SymIdent _ i _) = i

-- | Apply an AST expression to a list of expressions
apply :: Expression -> [Expression] -> Expression
apply = foldl Apply

-- | Negate an integer / a floating point literal
negateLit :: Literal -> Literal
negateLit lit = case lit of
  Int   s i -> Int   s (-i)
  Float s f -> Float s (-f)
  _         -> error "ExtendAbstractCurry.negateLit: NegativePattern"

-- | Get all patterns of left-hand-side
lhsPattern :: Lhs -> [Pattern]
lhsPattern lhs = flat lhs []
  where flat (FunLhs   _ ts) ts' = ts ++ ts'
        flat (OpLhs t1 _ t2) ts' = t1 : t2 : ts'
        flat (ApLhs lhs' ts) ts' = flat lhs' (ts ++ ts')

-- | Converts an infix operator to an expression
opToExpr :: InfixOp -> Expression
opToExpr (InfixOp    op) = Variable    op
opToExpr (InfixConstr c) = Constructor c

-- | Flatten local external declarations
flattenLocal :: [Decl] -> [Decl]
flattenLocal []     = []
flattenLocal (d:ds) = case d of
  (ExternalDecl is ss s) -> [ExternalDecl [i] ss s | i <- is] ++ flattenLocal ds
  _                      -> d : flattenLocal ds

-- | Add virtual spans of the form ((0,0),(0,0)) to type expressions
-- This function is used to extend all AbstractCurry functions for which
-- the type was inferred during AbstractCurry generation.
addVSpans :: AC.CTypeExpr -> ACS.CTypeExpr
addVSpans ty = case ty of
  AC.CTVar          tv -> ACS.CTVar virtualSpan (addVSpan tv)
  AC.CFuncType ty1 ty2 -> ACS.CFuncType virtualSpan (addVSpans ty1) (addVSpans ty2)
  AC.CTCons     qn tys -> ACS.CTCons virtualSpan (addVSpan qn) (map addVSpans tys)
 where
  addVSpan :: (a, b) -> (Span, a, b)
  addVSpan (x, y) = (virtualSpan, x, y)

convVis :: AC.CVisibility -> ACS.CVisibility
convVis AC.Public = ACS.Public
convVis AC.Private = ACS.Private

convFix :: AC.CFixity -> ACS.CFixity
convFix AC.CInfixOp = ACS.CInfixOp
convFix AC.CInfixlOp = ACS.CInfixlOp
convFix AC.CInfixrOp = ACS.CInfixrOp

convCas :: AC.CCaseType -> ACS.CCaseType
convCas AC.CRigid = ACS.CRigid
convCas AC.CFlex = ACS.CFlex

-- Since Curry allows the declaration non-adjacent function rules, e.g.
--
-- not True = False
--
-- square x = x * x
--
-- not False = True
--
-- we have to join all the declarations of a function in the AST representation,
-- before we can use this information to extend the AbstractCurry
-- representation.

-- | Join all equations of a function declaration
joinFunDecls :: [Decl] -> [Decl]
joinFunDecls = map join . groupBy ((==) `on` fName)
                             . sortBy  ((==) `on` fName)
  where
  join fs = case fs of
    [f]                            -> f
    (FunctionDecl f eq):funs@(_:_) -> FunctionDecl f (eq ++ concatMap fEqs funs)
    _                              -> error "ExtendAbstractCurry.joinFunDecls"

-- | Check if all rules of a function definition occur in adjacent order
adjacent :: [ACS.CRule] -> Bool
adjacent rs = check $ map getSpan rs
 where
  getSpan (ACS.CRule s _ _) = s
  check []                                 = True
  check [_]                                = True
  check ((_,(erow,_)):(s@((srow,_),_):ss)) = erow + 1 == srow && check (s:ss)

-- -----------------------------------------------------------------------------
-- Binding of AST declarations with span information
-- -----------------------------------------------------------------------------

-- We add all function, type and operator declarations included in the extended
-- AST to a finite map, respectively.
-- For function declarations we also story type information when present.

bindFun :: Decl -> FM String (Decl, Maybe TypeExpr) -> FM String (Decl, Maybe TypeExpr)
bindFun d fm = case d of
  FunctionDecl     i _ -> addToFM fm (idName i) (d, Nothing)
  ExternalDecl is ss s -> addListToFM fm
    [(symName i, ((ExternalDecl [i] ss s), Nothing)) | i <- is]
  _                    -> error "ExtendAbstractCurry.bindFun: no function declaration"

-- | add type information to function map
bindSig :: Decl -> FM String (Decl, Maybe TypeExpr) -> FM String (Decl, Maybe TypeExpr)
bindSig d fm = case d of
  TypeSig        is _ _ ty -> foldr addSig fm [(symName i, ty) | i <- is]
  ForeignDecl _ _ _ i _ ty -> addSig (symName i, ty) fm
  _                        -> error "ExtendAbstractCurry.bindSig: no type signature"

addSig :: (String, TypeExpr) -> FM String (Decl, Maybe TypeExpr) -> FM String (Decl, Maybe TypeExpr)
addSig (n, ty) fm = updFM fm n (\(d,_) -> (d, Just ty))

bindType :: Decl -> FM String Decl -> FM String Decl
bindType d fm = case d of
  DataDecl  _ i _ _ _ _ -> addToFM fm (idName i) d
  NewtypeDecl _ i _ _ _ -> addToFM fm (idName i) d
  TypeDecl    _ i _ _ _ -> addToFM fm (idName i) d
  _                     -> error "ExtendAbstractCurry.bindType: no type declaration"

bindOp :: Decl -> FM String Decl -> FM String Decl
bindOp d fm = case d of
  InfixDecl fix mprec ops ss ->
    addListToFM fm [(opName o, InfixDecl fix mprec [o] ss) | o <- ops]
  _                          -> error "ExtendAbstractCurry.bindOp: no operator declaration"
 where
  opName = idName . opIdent

-- | name of a parenthesized identifier
symName :: SymIdent -> String
symName (SymIdent _ i _) = idName i
