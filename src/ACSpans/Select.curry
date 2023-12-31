------------------------------------------------------------------------
--- This library provides some useful operations to select components
--- in AbstractCurry programs, i.e., it provides a collection of
--- selector functions for AbstractCurry.
---
--- @version May 2016
--- @category meta
------------------------------------------------------------------------

module ACSpans.Select
{-  ( progName, imports, functions, constructors, types, publicFuncNames
  , publicConsNames, publicTypeNames

  , typeName, typeVis, typeCons
  , consName, consVis
  , isBaseType, isPolyType, isFunctionalType, isIOType, isIOReturnType
  , argTypes, resultType, tvarsOfType, tconsOfType, modsOfType

  , funcName, funcArity, funcComment, funcVis, funcType, funcRules
  , ruleRHS, ldeclsOfRule

  , varsOfPat, varsOfExp, varsOfRhs, varsOfStat, varsOfLDecl
  , varsOfFDecl, varsOfRule

  , funcNamesOfLDecl, funcNamesOfFDecl, funcNamesOfStat
  , isPrelude
  )-} where

import ACSpans.AbstractCurrySpan
import List(union)


------------------------------------------------------------------------
-- Selectors for curry programs

-- Returns the name of a given Curry program.
progName :: CurryProg -> String
progName (CurryProg _ modname _ _ _ _) = modname

--- Returns the imports (module names) of a given Curry program.
imports :: CurryProg -> [MName]
imports (CurryProg _ _ ms _ _ _) = ms

--- Returns the function declarations of a given Curry program.
functions :: CurryProg -> [CFuncDecl]
functions (CurryProg _ _ _ _ fs _) = fs

--- Returns all constructors of given Curry program.
constructors :: CurryProg -> [CConsDecl]
constructors = concatMap typeCons . types

--- Returns the type declarations of a given Curry program.
types :: CurryProg -> [CTypeDecl]
types (CurryProg _ _ _ ts _ _) = ts

-- --- Returns the names of all visible functions in given Curry program.
publicFuncNames :: CurryProg -> [QName]
publicFuncNames = map funcName . filter ((== Public) .  funcVis) . functions

--- Returns the names of all visible constructors in given Curry program.
publicConsNames :: CurryProg -> [QName]
publicConsNames = map consName
                . filter ((== Public) .  consVis)
                . constructors

--- Returns the names of all visible types in given Curry program.
publicTypeNames :: CurryProg -> [QName]
publicTypeNames = map typeName . filter ((== Public) . typeVis) . types

------------------------------------------------------------------------
-- Selectors for type expressions

--- Returns the name of a given type declaration
typeName :: CTypeDecl -> QName
typeName (CType    _ n _ _ _) = n
typeName (CTypeSyn _ n _ _ _) = n
typeName (CNewType _ n _ _ _) = n

--- Returns the visibility of a given type declaration
typeVis :: CTypeDecl -> CVisibility
typeVis (CType    _ _ vis _ _) = vis
typeVis (CTypeSyn _ _ vis _ _) = vis
typeVis (CNewType _ _ vis _ _) = vis

--- Returns the constructors of a given type declaration.
typeCons :: CTypeDecl -> [CConsDecl]
typeCons (CType    _ _ _ _ cs) = cs
typeCons (CTypeSyn _ _ _ _ _ ) = []
typeCons (CNewType _ _ _ _ c ) = [c]

--- Returns the name of a given constructor declaration.
consName :: CConsDecl -> QName
consName (CCons   _ n _ _) = n
consName (CRecord _ n _ _) = n

--- Returns the visibility of a given constructor declaration.
consVis :: CConsDecl -> CVisibility
consVis (CCons   _ _ vis _) = vis
consVis (CRecord _ _ vis _) = vis

--- Returns true if the type expression is a base type.
isBaseType :: CTypeExpr -> Bool
isBaseType texp = case texp of
  CTCons _ _ args -> null args
  _               -> False

--- Returns true if the type expression contains type variables.
isPolyType :: CTypeExpr -> Bool
isPolyType (CTVar                _ _) = True
isPolyType (CFuncType _ domain range) = isPolyType domain || isPolyType range
isPolyType (CTCons      _ _ typelist) = any isPolyType typelist

--- Returns true if the type expression is a functional type.
isFunctionalType :: CTypeExpr -> Bool
isFunctionalType texp = case texp of
  CFuncType _ _ _ -> True
  _               -> False

--- Returns true if the type expression is (IO t).
isIOType :: CTypeExpr -> Bool
isIOType texp = case texp of
  CTCons _ tc _ -> tc == pre "IO"
  _           -> False

-- --- Returns true if the type expression is (IO t) with t/=() and
-- --- t is not functional
-- isIOReturnType :: CTypeExpr -> Bool
-- isIOReturnType (CTVar          _  _) = False
-- isIOReturnType (CFuncType     _ _ _) = False
-- isIOReturnType (CTCons _ tc typelist) =
--   tc==pre "IO" && head typelist /= CTCons (pre "()") []
--   && not (isFunctionalType (head typelist))

--- Returns all argument types from a functional type
argTypes :: CTypeExpr -> [CTypeExpr]
argTypes (CTVar         _ _) = []
argTypes (CTCons      _ _ _) = []
argTypes (CFuncType _ t1 t2) = t1 : argTypes t2

--- Return the result type from a (nested) functional type
resultType :: CTypeExpr -> CTypeExpr
resultType (CTVar          pos n) = (CTVar pos n)
resultType (CTCons pos  name args) = (CTCons pos name args)
resultType (CFuncType   _ _ t2) = resultType t2

--- Returns all type variables occurring in a type expression.
tvarsOfType :: CTypeExpr -> [CTVarIName]
tvarsOfType (CTVar         _ v) = [v]
tvarsOfType (CFuncType _ t1 t2) = tvarsOfType t1 ++ tvarsOfType t2
tvarsOfType (CTCons   _ _ args) = concatMap tvarsOfType args

--- Returns all type constructors used in the given type.
tconsOfType :: CTypeExpr -> [QName]
tconsOfType (CTVar          _ _) = []
tconsOfType (CFuncType  _ t1 t2) = tconsOfType t1 `union` tconsOfType t2
tconsOfType (CTCons  _ tc tys)   = foldr union [tc] $ map tconsOfType tys

-- --- Returns all modules used in the given type.
-- modsOfType :: CTypeExpr -> [String]
-- modsOfType = map fst . tconsOfType

------------------------------------------------------------------------
-- Selectors for function definitions

--- Returns the name of a given function declaration.
funcName :: CFuncDecl -> QName
funcName (CFunc   _  n _ _ _ _) = n
funcName (CmtFunc _ _ n _ _ _ _) = n
-- 
-- Returns the visibility of a given function declaration.
funcArity :: CFuncDecl -> Int
funcArity (CFunc    _ _ a _ _ _) = a
funcArity (CmtFunc _ _ _ a _ _ _) = a

--- Returns the documentation comment of a given function declaration.
funcComment :: CFuncDecl -> String
funcComment (CFunc     _ _ _ _ _ _) = ""
funcComment (CmtFunc _ cmt _ _ _ _ _) = cmt
-- 
--- Returns the visibility of a given function declaration.
funcVis :: CFuncDecl -> CVisibility
funcVis (CFunc   _  _ _ vis _ _) = vis
funcVis (CmtFunc _ _ _ _ vis _ _) = vis

--- Returns the type of a given function declaration.
funcType :: CFuncDecl -> CTypeExpr
funcType (CFunc     _ _ _ _ texp _) = texp
funcType (CmtFunc _ _ _ _ _ texp _) = texp

--- Returns the rules of a given function declaration.
funcRules :: CFuncDecl -> [CRule]
funcRules (CFunc     _ _ _ _ _ rules) = rules
funcRules (CmtFunc _ _ _ _ _ _ rules) = rules

------------------------------------------------------------------------
-- Selectors for rules.

--- Returns the right-hand side of a rules.
ruleRHS :: CRule -> CRhs
ruleRHS (CRule _ _ rhs) = rhs

--- Returns the local declarations of given rule.
ldeclsOfRule :: CRule -> [CLocalDecl]
ldeclsOfRule (CRule _ _ (CSimpleRhs  _ _ lDecls)) = lDecls
ldeclsOfRule (CRule _ _ (CGuardedRhs _ _ lDecls)) = lDecls

------------------------------------------------------------------------
-- Operations to compute the variables occurring in a pattern or expression:

--- Returns list of all variables occurring in a pattern.
--- Each occurrence corresponds to one element, i.e., the list might
--- contain multiple elements.
varsOfPat :: CPattern -> [CVarIName]
varsOfPat (CPVar _ v) = [v]
varsOfPat (CPLit _ _) = []
varsOfPat (CPComb _ _ pats) = concatMap varsOfPat pats
varsOfPat (CPAs _ v pat) = v : varsOfPat pat
varsOfPat (CPFuncComb _ _ pats) = concatMap varsOfPat pats
varsOfPat (CPLazy _ pat) = varsOfPat pat
varsOfPat (CPRecord _ _ recpats) = concatMap (varsOfPat . snd) recpats

--- Returns list of all variables occurring in an expression.
--- Each occurrence corresponds to one element, i.e., the list might
--- contain multiple elements.
varsOfExp :: CExpr -> [CVarIName]
varsOfExp (CVar _ v)            = [v]
varsOfExp (CLit _ _)            = []
varsOfExp (CSymbol _ _)         = []
varsOfExp (CApply _ e1 e2)      = varsOfExp e1 ++ varsOfExp e2
varsOfExp (CLambda _ pl le)     = concatMap varsOfPat pl ++ varsOfExp le
varsOfExp (CLetDecl _ ld le)    = concatMap varsOfLDecl ld ++ varsOfExp le
varsOfExp (CDoExpr _ sl)        = concatMap varsOfStat sl
varsOfExp (CListComp _ le sl)   = varsOfExp le ++ concatMap varsOfStat sl
varsOfExp (CCase _ _ ce bl)     =
  varsOfExp ce ++ concatMap (\ (p,rhs) -> varsOfPat p ++ varsOfRhs rhs) bl
varsOfExp (CTyped _ te _)       = varsOfExp te
varsOfExp (CRecConstr _ _ upds) = concatMap (varsOfExp . snd) upds
varsOfExp (CRecUpdate _ e upds) = varsOfExp e ++ concatMap (varsOfExp . snd) upds

--- Returns list of all variables occurring in a right-hand side.
--- Each occurrence corresponds to one element, i.e., the list might
--- contain multiple elements.
varsOfRhs :: CRhs -> [CVarIName]
varsOfRhs (CSimpleRhs _ rhs ldecls) =
  varsOfExp rhs ++ concatMap varsOfLDecl ldecls
varsOfRhs (CGuardedRhs _ gs  ldecls) =
  concatMap (\ (_,g,e) -> varsOfExp g ++ varsOfExp e) gs  ++
  concatMap varsOfLDecl ldecls

--- Returns list of all variables occurring in a statement.
--- Each occurrence corresponds to one element, i.e., the list might
--- contain multiple elements.
varsOfStat :: CStatement -> [CVarIName]
varsOfStat (CSExpr _ e)  = varsOfExp e
varsOfStat (CSPat _ p e) = varsOfPat p ++ varsOfExp e
varsOfStat (CSLet _ ld)  = concatMap varsOfLDecl ld

--- Returns list of all variables occurring in a local declaration.
--- Each occurrence corresponds to one element, i.e., the list might
--- contain multiple elements.
varsOfLDecl :: CLocalDecl -> [CVarIName]
varsOfLDecl (CLocalFunc _ f)     = varsOfFDecl f
varsOfLDecl (CLocalPat _ p rhs)  = varsOfPat p ++ varsOfRhs rhs
varsOfLDecl (CLocalVars _ lvars) = lvars

--- Returns list of all variables occurring in a function declaration.
--- Each occurrence corresponds to one element, i.e., the list might
--- contain multiple elements.
varsOfFDecl :: CFuncDecl -> [CVarIName]
varsOfFDecl (CFunc     _ _ _ _ _ r) = concatMap varsOfRule r
varsOfFDecl (CmtFunc _ _ _ _ _ _ r) = concatMap varsOfRule r

--- Returns list of all variables occurring in a rule.
--- Each occurrence corresponds to one element, i.e., the list might
--- contain multiple elements.
varsOfRule :: CRule -> [CVarIName]
varsOfRule (CRule _ pats rhs) = concatMap varsOfPat pats ++ varsOfRhs rhs

------------------------------------------------------------------------
-- Operations to compute the function names declared in functions, local
-- declarations and statements:

--- @return The declared function name of given local declaration in a list.
funcNamesOfLDecl :: CLocalDecl -> [QName]
funcNamesOfLDecl lDecl =
    case lDecl of CLocalFunc _ f -> funcNamesOfFDecl f
                  _              -> []

--- @return The declared function name of given function declaration in a list.
funcNamesOfFDecl :: CFuncDecl -> [QName]
funcNamesOfFDecl (CFunc     _ qn _ _ _ _) = [qn]
funcNamesOfFDecl (CmtFunc _ _ qn _ _ _ _) = [qn]

--- @return The declared function names of given statement in a list.
funcNamesOfStat :: CStatement -> [QName]
funcNamesOfStat stms =
    case stms of CSLet _ ld -> concatMap funcNamesOfLDecl ld
                 _          -> []

------------------------------------------------------------------------
--- Tests whether a module name is the prelude.
isPrelude :: String -> Bool
isPrelude m = m == "Prelude"

------------------------------------------------------------------------
