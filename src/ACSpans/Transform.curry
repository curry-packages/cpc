----------------------------------------------------------------------------
--- This library provides transformation and update operations
--- on AbstractCurry programs.
--- Since the transformations are defined recursively on structured types,
--- they are useful to construct specific transformations on AbstractCurry
--- programs.
--- In particular, this library contains the transformation
--- `renameCurryModule` to rename an AbstractCurry module.
---
--- @author Michael Hanus
--- @version April 2016
--- @category meta
----------------------------------------------------------------------------

module ACSpans.Transform where

import ACSpans.AbstractCurrySpan
import ACSpans.Select
import List (nub, union)
import ACSpans.Span

-- CurryProg

--- Transforms an AbstractCurry program.
trCProg :: (String -> [String] -> [CTypeDecl] -> [CFuncDecl] -> [COpDecl] -> a)
        -> CurryProg -> a
trCProg prog (CurryProg _ name imps types funcs ops) =
  prog name imps types funcs ops

----------------------------------------------------------------------------
-- CTypeDecl

--- Transforms a type declaration.
trCTypeDecl :: (QName -> CVisibility -> [CTVarIName] -> [CConsDecl] -> a)
            -> (QName -> CVisibility -> [CTVarIName] -> CTypeExpr  -> a)
            -> (QName -> CVisibility -> [CTVarIName] -> CConsDecl  -> a)
            -> CTypeDecl -> a
trCTypeDecl typ _ _   (CType _ name vis params cs)     = typ   name vis params cs
trCTypeDecl _ tsyn _  (CTypeSyn _ name vis params syn) = tsyn  name vis params syn
trCTypeDecl _ _ tntyp (CNewType _ name vis params nt)  = tntyp name vis params nt

----------------------------------------------------------------------------
-- CConsDecl

--- Transforms a constructor declaration.
trCConsDecl :: (QName -> CVisibility -> [CTypeExpr]  -> a)
            -> (QName -> CVisibility -> [CFieldDecl] -> a)
            -> CConsDecl -> a
trCConsDecl cons _ (CCons   _ name vis args) = cons name vis args
trCConsDecl _ rec  (CRecord _ name vis args) = rec  name vis args

----------------------------------------------------------------------------
-- CFieldDecl

--- Transforms a constructor declaration.
trCFieldDecl :: (QName -> CVisibility -> CTypeExpr  -> a)
             -> CFieldDecl -> a
trCFieldDecl field (CField _ name vis texp) = field name vis texp


----------------------------------------------------------------------------
-- CTypeExpr

--- Transforms a type expression.
trCTypeExpr :: (CTVarIName -> a)
            -> (QName -> [a] -> a)
            -> (a -> a -> a)
            -> CTypeExpr -> a
trCTypeExpr tvar tcons functype texp = trTE texp
 where
  trTE (CTVar _ n)           = tvar n
  trTE (CTCons _ name args)  = tcons name (map trTE args)
  trTE (CFuncType _ from to) = functype (trTE from) (trTE to)

----------------------------------------------------------------------------
-- COpDecl

--- Transforms an operator declaration.
trCOpDecl :: (QName -> CFixity -> Int -> a) -> COpDecl -> a
trCOpDecl op (COp _ name fix prec) = op name fix prec

----------------------------------------------------------------------------
-- CFuncDecl

--- Transforms a function declaration
trCFuncDecl :: (String -> QName -> Int -> CVisibility -> CTypeExpr -> [CRule] -> a)
            -> CFuncDecl -> a
trCFuncDecl func (CFunc   _    name arity vis t rs) = func "" name arity vis t rs
trCFuncDecl func (CmtFunc _ cm name arity vis t rs) = func cm name arity vis t rs

-- ----------------------------------------------------------------------------
-- CRule

--- Transform a rule.
trCRule :: ([CPattern] -> CRhs -> a) -> CRule -> a
trCRule rule (CRule _ pats rhs) = rule pats rhs

----------------------------------------------------------------------------
-- CRhs

--- Transforms a right-hand side (of a rule or case expression).
trCRhs :: (CExpr -> [CLocalDecl] -> a)
       -> ([(Span,CExpr, CExpr)] -> [CLocalDecl] -> a)
       -> CRhs -> a
trCRhs srhs _ (CSimpleRhs  _ exp   locals) = srhs exp locals
trCRhs _ grhs (CGuardedRhs _ gexps locals) = grhs gexps locals

----------------------------------------------------------------------------


--- Transforms a local declaration.
trCLocalDecl :: (CFuncDecl -> a)
             -> (CPattern -> CRhs -> a)
             -> ([CVarIName] -> a)
             -> CLocalDecl -> a
trCLocalDecl lfun _ _ (CLocalFunc _ fdecl)  = lfun fdecl
trCLocalDecl _ lpat _ (CLocalPat _ pat rhs) = lpat pat rhs
trCLocalDecl _ _ vars (CLocalVars _ vs)     = vars vs


----------------------------------------------------------------------------
-- CPattern

--- Transforms a pattern.
trCPattern :: (CVarIName -> a)
           -> (CLiteral -> a)
           -> (QName -> [a] -> a)
           -> (CVarIName -> a -> a)
           -> (QName -> [a] -> a)
           -> (QName -> [CField a] -> a)
           -> CPattern -> a
trCPattern fv fl fc fa ff fr pattern = trP pattern
 where
  trP (CPVar _ pvar)         = fv pvar
  trP (CPLit _ lit)          = fl lit
  trP (CPComb _ c pats)      = fc c (map trP pats)
  trP (CPAs _ v pat)         = fa v (trP pat)
  trP (CPFuncComb _ fn pats) = ff fn (map trP pats)
  trP (CPLazy _ pat)         = trP pat
  trP (CPRecord _ r fs)      = fr r (map (\(n,p) -> (n,trP p)) fs)


----------------------------------------------------------------------------
-- CExpr

--- Transforms an expression.
trExpr :: (CVarIName -> a)
       -> (CLiteral -> a)
       -> (QName -> a)
       -> (a -> a -> a)
       -> ([CPattern] -> a -> a)
       -> ([CLocalDecl] -> a -> a)
       -> ([CStatement] -> a)
       -> (a -> [CStatement] -> a)
       -> (CCaseType -> a -> [(CPattern, CRhs)] -> a)
       -> (a -> CTypeExpr -> a)
       -> (QName -> [CField a] -> a)
       -> (a -> [CField a] -> a)
       -> CExpr -> a
trExpr var lit sym app lam clet cdo lcomp cas typ rcon rupd exp = trE exp
 where
  trE (CVar _ n) = var n
  trE (CLit _ l) = lit l
  trE (CSymbol _ n) = sym n
  trE (CApply _ e1 e2) = app (trE e1) (trE e2)
  trE (CLambda _ pats e) = lam pats (trE e)
  trE (CLetDecl _ ls e) = clet ls (trE e)
  trE (CDoExpr _ stm) = cdo stm
  trE (CListComp _ e stm) = lcomp (trE e) stm
  trE (CCase _ ct e branches) = cas ct (trE e) branches
  trE (CTyped _ e te) = typ (trE e) te
  trE (CRecConstr _ rn fds) = rcon rn (map (\ (lb,e) -> (lb, trE e)) fds)
  trE (CRecUpdate _ e  fds) = rupd (trE e) (map (\ (lb,v) -> (lb, trE v)) fds)

----------------------------------------------------------------------------
-- CStatement

--- Transforms a statement (occuring in do expressions or list comprehensions).
trCStatement :: (CExpr -> a)
             -> (CPattern -> CExpr -> a)
             -> ([CLocalDecl] -> a)
             -> CStatement -> a
trCStatement sexp _ _ (CSExpr _ exp)    = sexp exp
trCStatement _ spat _ (CSPat _ pat exp) = spat pat exp
trCStatement _ _ slet (CSLet _ locals)  = slet locals

-------------------------------------------------------------------------
--- Extracts all type names occurring in a program.
typesOfCurryProg :: CurryProg -> [QName]
typesOfCurryProg =
  trCProg (\_ _ types funcs _ ->
              foldr union [] (map (nub . typesOfCTypeDecl) types) ++
              foldr union [] (map (nub . typesOfCFuncDecl) funcs))

--- Extracts all type names occurring in a type declaration.
typesOfCTypeDecl :: CTypeDecl -> [QName]
typesOfCTypeDecl =
  trCTypeDecl (\qn _ _ cdecls -> qn : concatMap typesOfConsDecl cdecls)
              (\qn _ _ texp   -> qn : typesOfTypeExpr texp)
              (\qn _ _ cdecl  -> qn : typesOfConsDecl cdecl)

typesOfConsDecl :: CConsDecl -> [QName]
typesOfConsDecl =
  trCConsDecl (\_ _ texps   -> concatMap typesOfTypeExpr texps)
              (\_ _ fddecls -> concatMap typesOfFieldDecl fddecls)

typesOfFieldDecl :: CFieldDecl -> [QName]
typesOfFieldDecl = trCFieldDecl (\_ _ texp -> typesOfTypeExpr texp)

typesOfTypeExpr :: CTypeExpr -> [QName]
typesOfTypeExpr = trCTypeExpr (\_ -> [])
                              (\qn targs -> qn : concat targs)
                              (++)

typesOfCFuncDecl :: CFuncDecl -> [QName]
typesOfCFuncDecl =
  trCFuncDecl (\_ _ _ _ texp _ -> typesOfTypeExpr texp)
--   type annotations in expressions are currently ignored

----------------------------------------------------------------------------
--- Extracts all function (and constructor) names occurring in a program.
funcsOfCurryProg :: CurryProg -> [QName]
funcsOfCurryProg =
  trCProg (\_ _ types funcs _ ->
              foldr union [] (map (nub . funcsOfCTypeDecl) types) ++
              foldr union [] (map (nub . funcsOfCFuncDecl) funcs))

funcsOfCTypeDecl :: CTypeDecl -> [QName]
funcsOfCTypeDecl =
  trCTypeDecl (\_ _ _ cdecls -> concatMap funcsOfConsDecl cdecls)
              (\_ _ _ _      -> [])
              (\_ _ _ cdecl  -> funcsOfConsDecl cdecl)

funcsOfConsDecl :: CConsDecl -> [QName]
funcsOfConsDecl =
  trCConsDecl (\qn _ _       -> [qn])
              (\qn _ fddecls -> qn : concatMap funcsOfFieldDecl fddecls)

funcsOfFieldDecl :: CFieldDecl -> [QName]
funcsOfFieldDecl = trCFieldDecl (\qn _ _ -> [qn])

--- Extracts all function (and constructor) names occurring in a function
--- declaration.
funcsOfCFuncDecl :: CFuncDecl -> [QName]
funcsOfCFuncDecl =
  trCFuncDecl (\_ _ _ _ _ rules -> concatMap funcsOfCRule rules)

funcsOfCRule :: CRule -> [QName]
funcsOfCRule = trCRule (\_ rhs -> funcsOfCRhs rhs)

funcsOfCRhs :: CRhs -> [QName]
funcsOfCRhs =
  trCRhs (\e  ldecls -> funcsOfExpr e ++ concatMap funcsOfLDecl ldecls)
         (\gs ldecls -> concatMap (\ (_,g,e) -> funcsOfExpr g ++ funcsOfExpr e) gs
                        ++ concatMap funcsOfLDecl ldecls)

funcsOfLDecl :: CLocalDecl -> [QName]
funcsOfLDecl = trCLocalDecl funcsOfCFuncDecl (const funcsOfCRhs) (const [])

funcsOfExpr :: CExpr -> [QName]
funcsOfExpr =
  trExpr (const [])
         (const [])
         (\n -> [n])
         (++)
         (const id)
         (\ldecls e -> concatMap funcsOfLDecl ldecls ++ e)
         (concatMap funcsOfStat)
         (\e stats -> e ++ concatMap funcsOfStat stats)
         (\_ e brs -> e ++ concatMap (funcsOfCRhs . snd) brs)
         (\e _ -> e)
         (\_ fields -> concatMap snd fields)
         (\e fields -> e ++ concatMap snd fields)

funcsOfStat :: CStatement -> [QName]
funcsOfStat = trCStatement funcsOfExpr
                           (const funcsOfExpr)
                           (concatMap funcsOfLDecl)

----------------------------------------------------------------------------
--- Renames a Curry module, i.e., updates the module name and all qualified
--- names in a program.
type Update a b = (b -> b) -> a -> a

renameCurryModule :: String -> CurryProg -> CurryProg
renameCurryModule newname prog =
  updCProgName (const newname) (updQNamesInCProg rnm prog)
 where
  rnm mn@(p,mod,n) | mod == progName prog = (p,newname,n)
                   | otherwise            = mn

-- ----------------------------------------------------------------------------
--- Updates an AbstractCurry program.
updCProg :: (String      -> String)      ->
            ([String]    -> [String])    ->
            ([CTypeDecl] -> [CTypeDecl]) ->
            ([CFuncDecl] -> [CFuncDecl]) ->
            ([COpDecl]   -> [COpDecl])   -> CurryProg -> CurryProg
updCProg fn fi ft ff fo f@(CurryProg pos _ _ _ _ _) = trCProg prog f
 where
  prog name imps types funcs ops =
    CurryProg pos (fn name) (fi imps) (ft types) (ff funcs) (fo ops)

 --- Updates the name of a Curry program.
updCProgName :: Update CurryProg String
updCProgName f = updCProg f id id id id

--- update type declaration
updCTypeDecl :: (QName -> QName)
             -> (CVisibility  -> CVisibility)
             -> ([CTVarIName] -> [CTVarIName])
             -> ([CConsDecl]  -> [CConsDecl])
             -> (CTypeExpr    -> CTypeExpr)
             -> (CConsDecl    -> CConsDecl)
             -> CTypeDecl -> CTypeDecl
updCTypeDecl fn fv fp fc fs ft f = trCTypeDecl typ tsyn tntyp f
 where
  pos = getPosType f
  typ   name vis params cs   = CType    pos (fn name) (fv vis) (fp params) (fc cs)
  tsyn  name vis params syn  = CTypeSyn pos (fn name) (fv vis) (fp params) (fs syn)
  tntyp name vis params ntyp = CNewType pos (fn name) (fv vis) (fp params) (ft ntyp)

getPosType:: CTypeDecl -> Span
getPosType (CType pos _ _ _ _)    = pos
getPosType (CTypeSyn pos _ _ _ _) = pos
getPosType (CNewType pos _ _ _ _) = pos

--- Updates the name of a type declaration.
updCTypeDeclName :: Update CTypeDecl QName
updCTypeDeclName f = updCTypeDecl f id id id id id

--- Updates a constructor declaration.
updCConsDecl :: (QName -> QName)
             -> (CVisibility  -> CVisibility)
             -> ([CTypeExpr]  -> [CTypeExpr])
             -> ([CFieldDecl] -> [CFieldDecl])
             -> CConsDecl -> CConsDecl
updCConsDecl fn fv fts ffs f = trCConsDecl cons rec f
 where
  pos = getPosCons f
  cons name vis args = CCons pos (fn name) (fv vis) (fts args)
  rec  name vis args = CRecord pos (fn name) (fv vis) (ffs args)

getPosCons:: CConsDecl -> Span
getPosCons (CCons pos _ _ _)   = pos
getPosCons (CRecord pos _ _ _) = pos

--- Updates the name of a constructor declaration.
updCConsDeclName :: Update CConsDecl QName
updCConsDeclName f = updCConsDecl f id id id

--- update constructor declaration
updCFieldDecl :: (QName -> QName)
              -> (CVisibility -> CVisibility)
              -> (CTypeExpr   -> CTypeExpr)
              -> CFieldDecl -> CFieldDecl
updCFieldDecl fn fv ft f@(CField pos _ _ _) = trCFieldDecl field f
 where
  field name vis texp = CField pos (fn name) (fv vis) (ft texp)

--- Updates the name of a constructor declaration.
updCFieldDeclName :: Update CFieldDecl QName
updCFieldDeclName f = updCFieldDecl f id id

--- Updates all type constructor applications in a type expression.
updTConsApp :: (QName -> [CTypeExpr] -> CTypeExpr) -> CTypeExpr -> CTypeExpr
updTConsApp tcons ctexp = trCTypeExpr (CTVar posi) tcons (CFuncType posi) ctexp
 where posi = getPosTExpr ctexp
       getPosTExpr (CTVar pos _)       = pos
       getPosTExpr (CFuncType pos _ _) = pos
       getPosTExpr (CTCons pos _ _)    = pos---- WHAT


--- Updates an operator declaration.
updCOpDecl :: (QName -> QName) -> (CFixity -> CFixity) -> (Int -> Int)
           -> COpDecl -> COpDecl
updCOpDecl fn ff fp c@(COp pos _ _ _) = trCOpDecl op c
 where
  op name fix prec = COp pos (fn name) (ff fix) (fp prec)

--- Updates the name of an operator declaration.
updCOpName :: Update COpDecl QName
updCOpName f = updCOpDecl f id id

--- Updates a function declaration.
updCFuncDecl :: (String -> String)
             -> (QName -> QName)
             -> (Int -> Int)
             -> (CVisibility -> CVisibility)
             -> (CTypeExpr -> CTypeExpr)
             -> ([CRule] -> [CRule])
             -> CFuncDecl -> CFuncDecl

updCFuncDecl fc fn fa fv ft fr f = trCFuncDecl func f
 where 
  func cmt name arity vis t rules =
    if null cmt
    then CFunc (getPosFunc f) (fn name) (fa arity) (fv vis) (ft t) (fr rules)
    else CmtFunc (getPosFunc f) (fc cmt) (fn name) (fa arity) (fv vis) (ft t) (fr rules)
    
getPosFunc:: CFuncDecl -> (Maybe Span)
getPosFunc (CFunc   pos _ _ _ _ _)   = pos  
getPosFunc (CmtFunc pos _ _ _ _ _ _) = pos


--- Update a rule.
updCRule :: ([CPattern] -> [CPattern])
         -> (CRhs -> CRhs)
         -> CRule -> CRule
updCRule fp fr r@(CRule pos _ _) = trCRule rule r
 where
  rule pats rhs = CRule pos (fp pats) (fr rhs)

--- Updates right-hand side.
updCRhs :: (CExpr -> CExpr)
        -> ([(Span,CExpr, CExpr)] -> [(Span,CExpr, CExpr)])
        -> ([CLocalDecl]     -> [CLocalDecl])
        -> CRhs -> CRhs
updCRhs fe fg fl r@(CSimpleRhs pos _ _) = trCRhs srhs grhs r
 where
  srhs exp   locals = CSimpleRhs pos (fe exp) (fl locals)
  grhs gexps locals = CGuardedRhs pos (fg gexps) (fl locals)

updCRhs fe fg fl r@(CGuardedRhs pos _ _) = trCRhs srhs grhs r
 where
  srhs exp   locals = CSimpleRhs pos (fe exp) (fl locals)
  grhs gexps locals = CGuardedRhs pos (fg gexps) (fl locals)

--- Updates a local declaration.
updCLocalDecl :: (CFuncDecl   -> CFuncDecl)
              -> (CPattern    -> CPattern)
              -> (CRhs        -> CRhs)
              -> ([CVarIName] -> [CVarIName])
              -> CLocalDecl   -> CLocalDecl
updCLocalDecl ff fp fr fv f = trCLocalDecl lfun lpat lvars f
 where
  pos = getPos f
  lfun fdecl   = CLocalFunc pos (ff fdecl)
  lpat pat rhs = CLocalPat  pos (fp pat) (fr rhs)
  lvars vars   = CLocalVars pos (fv vars)

getPos:: CLocalDecl -> Span
getPos (CLocalFunc pos _  ) = pos
getPos (CLocalPat  pos _ _) = pos
getPos (CLocalVars pos _  ) = pos

--- Updates a pattern.
updCPattern :: (CVarIName   -> CVarIName)
            -> (CLiteral    -> CLiteral)
            -> (QName       -> QName)
            -> CPattern   -> CPattern
updCPattern fv fl fn pati = trCPattern pvar plit pcomb pas pfcomb prec pati
 where
  pos = getPosPat pati
  pvar var = CPVar pos (fv var)
  plit lit = CPLit pos (fl lit)
  pcomb c pats = CPComb pos (fn c) (map (updCPattern fv fl fn) pats)
  pas v pat = CPAs pos (fv v) (updCPattern fv fl fn pat)
  pfcomb f pats = CPFuncComb pos (fn f) (map (updCPattern fv fl fn) pats)
  prec r fields = CPRecord pos (fn r)
                    (map (\ (n,p) -> (fn n, updCPattern fv fl fn p)) fields)

getPosPat:: CPattern -> Span
getPosPat (CPVar  pos _ )      = pos
getPosPat (CPLit  pos _)       = pos
getPosPat (CPComb pos _ _)     = pos
getPosPat (CPAs   pos _ _)     = pos
getPosPat (CPFuncComb pos _ _) = pos
getPosPat (CPRecord   pos _ _) = pos
getPosPat (CPLazy pos _)       = pos


--- Updates a statement (occuring in do expressions or list comprehensions).
updCStatement :: (CExpr      -> CExpr)
              -> (CPattern   -> CPattern)
              -> (CLocalDecl -> CLocalDecl)
              -> CStatement  -> CStatement
updCStatement fe fp fd f = trCStatement sexp spat slet f
 where
  pos = getPosStat f
  sexp exp     = CSExpr pos (fe exp)
  spat pat exp = CSPat  pos (fp pat) (fe exp)
  slet locals  = CSLet  pos (map fd locals)

getPosStat:: CStatement -> Span
getPosStat (CSExpr pos _)   = pos
getPosStat (CSPat  pos _ _) = pos
getPosStat (CSLet  pos _)    = pos

----------------------------------------------------------------------------


--- Updates all qualified names in a Curry program.
updQNamesInCProg :: Update CurryProg QName
updQNamesInCProg f =
  updCProg id
           id
           (map (updQNamesInCTypeDecl f))
           (map (updQNamesInCFuncDecl f))
           (map (updCOpName f))

--- Updates all qualified names in a type declaration.
updQNamesInCTypeDecl :: Update CTypeDecl QName
updQNamesInCTypeDecl f =
  updCTypeDecl f id id
               (map (updQNamesInCConsDecl f))
               (updQNamesInCTypeExpr f)
               (updQNamesInCConsDecl f)

--- Updates all qualified names in a constructor declaration.
updQNamesInCConsDecl :: Update CConsDecl QName
updQNamesInCConsDecl f =
  updCConsDecl f id
               (map (updQNamesInCTypeExpr f))
               (map (updQNamesInCFieldDecl f))

--- Updates all qualified names in a record field declaration.
updQNamesInCFieldDecl :: Update CFieldDecl QName
updQNamesInCFieldDecl f = updCFieldDecl f id (updQNamesInCTypeExpr f)

--- Updates all qualified names in a type expression.
updQNamesInCTypeExpr :: (QName -> QName) -> CTypeExpr -> CTypeExpr
updQNamesInCTypeExpr f c = updTConsApp (\name args -> CTCons posi (f name) args) c
   where posi = getPosTExpr c
         getPosTExpr (CTVar pos _)       = pos
         getPosTExpr (CFuncType pos _ _) = pos
         getPosTExpr (CTCons pos _ _)    = pos

--- Updates all qualified names in a function declaration.
updQNamesInCFuncDecl :: Update CFuncDecl QName
updQNamesInCFuncDecl f =
  updCFuncDecl id f id id (updQNamesInCTypeExpr f) (map (updQNamesInCRule f))

--- Updates all qualified names in a function declaration.
updQNamesInCRule :: Update CRule QName
updQNamesInCRule f =
  updCRule (map (updQNamesInCPattern f))
           (updQNamesInCRhs f)

--- Updates all qualified names in a function declaration.
updQNamesInCRhs :: Update CRhs QName
updQNamesInCRhs f =
  updCRhs (updQNamesInCExpr f)
          (map (\ (p,g,e) -> (p,updQNamesInCExpr f g, updQNamesInCExpr f e)))
          (map (updQNamesInCLocalDecl f))

--- Updates all qualified names in a function declaration.
updQNamesInCLocalDecl :: Update CLocalDecl QName
updQNamesInCLocalDecl f =
  updCLocalDecl (updQNamesInCFuncDecl f)
                (updQNamesInCPattern f)
                (updQNamesInCRhs f)
                id

--- Updates all qualified names in a function declaration.
updQNamesInCPattern :: Update CPattern QName
updQNamesInCPattern f = updCPattern id id f

--- Updates all qualified names in a statement.
updQNamesInCStatement :: Update CStatement QName
updQNamesInCStatement f =
  updCStatement (updQNamesInCExpr f)
                (updQNamesInCPattern f)
                (updQNamesInCLocalDecl f)


vir:: Span
vir = ((1,1),(1,1))                

updQNamesInCExpr :: Update CExpr QName
updQNamesInCExpr f cexp =
  trExpr (CVar pos) (CLit pos) ((CSymbol pos). f) (CApply pos) lam ldecl doexp lcomp ccase ctyped
         reccon recupd cexp
 where
  pos = getPosExp cexp
  lam pats exp = CLambda pos (map (updQNamesInCPattern f) pats) exp
  ldecl locals exp = CLetDecl pos (map (updQNamesInCLocalDecl f) locals) exp
  doexp stms = CDoExpr pos (map (updQNamesInCStatement f) stms)
  lcomp exp stms = CListComp pos exp (map (updQNamesInCStatement f) stms)
  ccase ct exp bs = CCase pos ct exp
    (map (\ (pat,rhs) -> (updQNamesInCPattern f pat, updQNamesInCRhs f rhs)) bs)
  ctyped exp texp = CTyped pos exp (updQNamesInCTypeExpr f texp)
  reccon rec fields = CRecConstr pos (f rec) (map (\ (l,e) -> (f l,e)) fields)
  recupd exp fields = CRecUpdate pos exp (map (\ (l,e) -> (f l,e)) fields)

getPosExp:: CExpr -> Span
getPosExp (CLambda pos _ _)    = pos
getPosExp (CLetDecl pos _ _)   = pos
getPosExp (CDoExpr pos _)      = pos
getPosExp (CListComp pos _ _)  = pos
getPosExp (CCase pos _ _ _)    = pos
getPosExp (CTyped pos _ _)     = pos
getPosExp (CRecConstr pos _ _) = pos
getPosExp (CRecUpdate pos _ _) = pos
getPosExp (CVar pos _)         = pos
getPosExp (CLit pos _)         = pos
getPosExp (CSymbol pos _)      = pos
getPosExp (CApply pos _ _)     = pos



-------------------------------------------------------------------------
