module CPC.Instrumentation
  ( startTransform, transExp, Opts )
 where


import ACSpans.AbstractCurrySpan
import AbstractCurry.Pretty (showCProg)
import ACSpans.Span         (Span)
import ACSpans.Select       (progName)
import FiniteMap            (FM,emptyFM,isEmptyFM,lookupWithDefaultFM)
import Maybe                (isJust)
import Unsafe               (unsafePerformIO)
import INS                  ( INS
                            , bindS
                            , bindS_
                            , putS
                            , returnS
                            , liftS
                            , liftS
                            , liftS2
                            , updateID
                            , mapS
                            , getS
                            , runState)
import CMeta ( BoxEntry
             , BoxLabel
             , mkExpBox
             , mkTopBox
             , mkifTBox
             , mkifFBox
             , mkGuardTBox
             , mkGuardFBox 
             , mkLocBox)

import CPC.Files ( addInstSuffix )             

------------------------------------------------------------------------------
initID:: Int
initID = 0
initBoxList:: [BoxEntry]
initBoxList = []

data Opts = Opts
     { optim  :: Bool
     , arityM :: FM (String, String) Int
       }
          
defaultOpts :: Opts
defaultOpts =
    Opts {
      optim = False
    , arityM = emptyFM (<)
      }
          
setOptim :: FM (String,String) Int -> Opts -> Opts
setOptim arM o = if isEmptyFM arM
                 then defaultOpts
                 else o {optim = True, arityM = arM}

------------------------------------------------------------------------------
startTransform :: Int -> CurryProg -> [String] -> Bool -> Bool 
              -> (FM (String,String) Int) -> (CurryProg,(Int, [BoxEntry]))
startTransform id prog imps rename transMain arMap = 
 ((CurryProg pos modname nMods t f o),(info,boxes))
  where timps mo        = if mo `elem` ["Prelude","CLib"] 
                            then mo 
                            else if mo `elem` imps 
                                 then addInstSuffix mo
                                 else mo
        modname         = if rename then mname else mname
        impnames        = if imps==[] then m else map timps m
        nMods           = "CLib":impnames
        transProg       = transCP (setOptim arMap defaultOpts) prog 
     --   repl c          = if c == '/' then '.' else c
        ((CurryProg pos mname m t f o),(info,boxes)) = if transMain 
                                                         then (prog, (1,[])) 
                                                         else runState 
                                                              transProg (id,[])

-- CurryProg
transCP :: Opts -> CurryProg -> INS CurryProg
transCP o (CurryProg pos mname mnames tdecls fdecls odecls) = 
  liftS2 ((CurryProg pos mname mnames tdecls)) transFuncs transDecls
  where transFuncs = mapS (transFunc o True) fdecls
        transDecls = transO odecls

                                                      

transO :: [COpDecl] -> INS [COpDecl]
transO oDecls = returnS oDecls


-- Functions
transFunc :: Opts -> Bool -> CFuncDecl -> INS CFuncDecl
transFunc o isTop (CFunc   pos   qn@(p, qname,name) ar vis te rules) =
  let trules = (mapS (transRule o alts) rules)
      alts   = if length rules > 1 then True else False
  in  (updateID pos qn isTop) 
      `bindS_` -- applyticktransformation
      (CFunc   pos   (p,(qname),name) ar vis te `liftS` trules)
transFunc o isTop (CmtFunc pos s qn@(p, qname,name) ar vis te rules) = 
  let trules = (mapS (transRule o alts) rules)
      alts   = if length rules > 1 then True else False
  in  (updateID pos qn isTop) `bindS_`
      (CmtFunc pos s (p,(qname),name) ar vis te `liftS` trules) 

--transRules :: [CRule] -> INS [CRule]
--transRules [] = returnS []
--transRules (rule:xs) = let tRule = `liftS` transRule rule
--                      in tRule:(transRules xs)

-- Rules
transRule :: Opts -> Bool -> CRule -> INS CRule
transRule o multipleAlts (CRule pos pats rhs) = 
  let trhs = transRhs o multipleAlts rhs
  in CRule pos pats `liftS` trhs -- TODO Pats seperating

-- Rhs TODO ALTERNATIVES just one
transRhs :: Opts -> Bool -> CRhs -> INS CRhs
transRhs o multipleAlts (CSimpleRhs  pos exp  lodecls)   = 
  let texp       = transExp o multipleAlts exp
      transDecls = mapS (transLocDecl o) lodecls
  in  liftS2 (CSimpleRhs  pos) texp transDecls
transRhs o           _ (CGuardedRhs pos exps lodecls)   = 
  let texp = mapS (transGuard o alts) exps
      alts = if length exps > 1 then True else False
  in  liftS2 (CGuardedRhs pos) texp (mapS (transLocDecl o) lodecls) -- TODO BOOLTICKS


-- (CApply (CApply (CApply (CSymbol ("Prelude","if_then_else")) 
-- (CApply (CApply (CSymbol ("Prelude","==")) (CVar (3,"r"))) (CLit (CIntc 0)))) 

-- Guards;
transGuard :: Opts -> Bool -> (Span, CExpr, CExpr) 
           -> INS (Span, CExpr, CExpr)
transGuard o multipleAlts (pos, exp1, exp2) = liftS2 ((,,) pos) 
                                              ((transExp o False exp1) 
                                              `bindS`
                                              applyGuardTick) 
                                              (transExp o multipleAlts exp2) -- BOOL

  
-- Expressions
transExp :: Opts -> Bool -> CExpr -> INS CExpr
transExp _ isAlt e@(CVar pos _)               = instrument isAlt pos e                                                  
transExp _ isAlt e@(CLit pos _)               = instrument isAlt pos e
transExp _ isAlt e@(CSymbol pos _)            = instrument isAlt pos e
transExp o isAlt e@(CApply _ _ _)             = if optim o
                                                  then handleCApply' o isAlt e
                                                  else handleCApply  o isAlt  e
transExp o isAlt (CLambda pos cp exp)         = instrumentWithExp isAlt pos 
                                                (((CLambda pos) cp) 
                                                `liftS` 
                                                (transExp o isAlt exp))
transExp o isAlt (CLetDecl pos lodecls exp)  = liftS2 (CLetDecl pos) 
                                               (mapS (transLocDecl o) 
                                                             lodecls) 
                                               (transExp o isAlt exp)
transExp o isAlt (CDoExpr pos stmts)         = instrumentWithExp isAlt pos 
                                               ((CDoExpr pos) 
                                               `liftS` 
                                               (mapS (transStatement o False) 
                                                                               stmts))
transExp o isAlt (CListComp pos exp stmts)   = instrumentWithExp isAlt pos 
                                               (liftS2 (CListComp pos)
                                               (transExp o False exp) 
                                               (mapS (transStatement o False) stmts))
transExp o isAlt (CCase pos cct exp pr)      = instrumentWithExp isAlt pos 
                                               (liftS2 (CCase pos cct) 
                                               (transExp o isAlt exp) 
                                               (mapS (transCasePat o) pr))                                                                
transExp o isAlt (CTyped pos exp txp)        = instrumentWithExp isAlt pos 
                                               (liftS2 (CTyped pos) 
                                               (transExp o False exp) 
                                               (returnS txp)) -- typed expression
transExp o isAlt (CRecConstr pos qn fields)  = instrumentWithExp isAlt pos 
                                               ((CRecConstr pos qn) 
                                               `liftS` 
                                               (mapS (transField o) fields))
transExp o isAlt (CRecUpdate pos exp fields) = instrumentWithExp isAlt pos 
                                               (liftS2 (CRecUpdate pos) 
                                               (transExp o False exp) 
                                               (mapS (transField o) fields))

------------------------------------------------------------------------------------

transStatement:: Opts-> Bool -> CStatement -> INS CStatement
transStatement o isAlt (CSExpr pos exp)     = (CSExpr pos)     
                                              `liftS` (transExp o isAlt exp)
transStatement o isAlt (CSPat  pos pat exp) = (CSPat  pos pat) 
                                               `liftS` (transExp o isAlt exp)
transStatement o _     (CSLet  pos locs)    = (CSLet  pos)     `
                                               liftS` (mapS (transLocDecl o) locs)

transField:: Opts -> (QName, CExpr) -> INS (QName, CExpr)
transField o (qn , exp) = (,) qn `liftS` (transExp o False exp)

--transBool :: CExpr -> INS CExpr
--transBool (CApply (CSymbol ("Prelude","if_then_else")) exp2) = 
-------------------------------------------------------------------------------------

-- TODO LOCALBOXES
transLocDecl :: Opts -> CLocalDecl -> INS CLocalDecl
transLocDecl o (CLocalFunc pos fdecl)   = (CLocalFunc pos) 
                                          `liftS` (transFunc o False fdecl)
transLocDecl o (CLocalPat  pos pat rhs) = (CLocalPat pos pat) 
                                          `liftS` (transRhs o False rhs)
transLocDecl _ (CLocalVars pos vars)    = returnS (CLocalVars pos vars)

transCasePat :: Opts -> (CPattern, CRhs) -> INS (CPattern, CRhs)
transCasePat o (pat, rhs) = (,) pat `liftS` (transRhs o True rhs)



-------------------------------------------------------------------------------------
d :: Span 
d = ((1,0),(0,0))

applyTick :: Span -> Int -> CExpr -> CExpr
applyTick pos id prog = (CApply pos 
                          (CApply pos 
                             (CSymbol pos (pos, "CLib","tick")) 
                             (CLit pos (CIntc pos id))) 
                         prog)

applyBoolTick :: CExpr -> Span -> CExpr -> INS CExpr
applyBoolTick exp1 posi exp2 = 
  if isIf(exp1) 
    then (getS 
         `bindS` \(id,boxes)->
         putS (id+2, (id, posi, mkifTBox):
         (id+1, posi, mkifFBox):boxes) 
         `bindS_`
         returnS (CApply posi 
                   (CApply posi 
                     (CApply posi 
                       (CSymbol posi (posi, "CLib","boolTick")) 
                       (CLit posi (CIntc posi id))) 
                     (CLit posi (CIntc posi (id+1)))) 
                   exp2))
    else returnS exp2
      where isIf exp = case exp of
                   (CSymbol _ (_, "Prelude","if_then_else")) -> True
                   _                                         -> False
            --pos = getPosExp exp2

applyGuardTick:: CExpr -> INS CExpr
applyGuardTick exp1 = {-returnS exp1-}
  (getS 
   `bindS` \(id,boxes)->
   putS (id+2, (id, posi, mkGuardTBox):
   (id+1, posi, mkGuardFBox):boxes)
   `bindS_`
   returnS (CApply posi 
              (CApply posi 
                 (CApply posi 
                 (CSymbol posi (posi, "CLib","boolTick")) 
                 (CLit posi (CIntc posi id))) 
              (CLit posi (CIntc posi (id+1)))) 
            exp1))
  where posi = getPosExp exp1 

------------------------------------------------------------------------------ 
  
instrument:: Bool -> Span -> CExpr -> INS CExpr
instrument isAlt pos exp =   getS 
                            `bindS` \(id,boxes)-> 
                             putS ( id+1
                                , (id, pos, mkExpBox isAlt):boxes) 
                            `bindS_` returnS (applyTick pos id exp)     

instrumentWithExp:: Bool -> Span -> INS CExpr -> INS CExpr
instrumentWithExp isAlt pos exp = getS 
                                  `bindS` \(id,boxes)-> 
                                   putS ( id+1
                                       , (id, pos, mkExpBox isAlt):boxes) 
                                  `bindS_` (applyTick pos id) `liftS` exp   

                                  
-------------------------------------------------------------------------------                              
                                  
getPosExp :: CExpr -> Span
getPosExp (CVar pos _)         = pos                                                 
getPosExp (CLit pos _)         = pos
getPosExp (CSymbol pos _)      = pos
getPosExp (CApply pos _ e2)    = if pos == d then getPosExp e2 else pos
getPosExp (CLambda pos _ _)    = pos
getPosExp (CLetDecl pos _ _)   = pos
getPosExp (CDoExpr pos _)      = pos
getPosExp (CListComp pos _ _)  = pos
getPosExp (CCase pos _ _ _)    = pos                                                              
getPosExp (CTyped pos _ _)     = pos
getPosExp (CRecConstr pos _ _) = pos
getPosExp (CRecUpdate pos _ _) = pos
                   
-- applyBoolTick exp1 nm exp2 = returnS exp2

-------------------------------------------------------------------------------

-- transformBack :: CurryProg -> String
--transformBack cprog =  showCProg cprog

-------------------------------------------------------------------------------
handleCApply':: Opts -> Bool -> CExpr -> INS CExpr
handleCApply' o isAlt cexp@(CApply pos _ _) = 
  let flat     = (flatApply cexp [])
      flatFunc = map snd flat
      possi    = map fst flat
      poss     = tail possi
      (CSymbol _ (_,mname,qname)) = head flatFunc
      ar       = {-if isITE cexp then 3 else-} lookupWithDefaultFM (arityM o) 0 (mname,qname)
      params   = take ar (tail flatFunc)
--     (posi, noPosi)       = splitAt ar poss
      tickedParams = mapS (transExp o False) params
--     tickedNoParams = mapS (transExp o False) noParams
   in if(isITE cexp || flat == []) 
       then handleCApply o isAlt cexp 
       else if ar==length(tail flatFunc)  
              then (getS 
                   `bindS` \(id,boxes)-> 
                    putS (id+1, (id, pos, mkExpBox isAlt):
                                (map (dummyBoxes id) possi)++boxes) 
                    `bindS_`(applyTick pos id) 
                    `liftS` ((buildCApply (head flatFunc) poss) 
                            `liftS` tickedParams))
        else handleCApply o isAlt cexp
handleCApply' _ _ (CSymbol _ _)                 = error "handleApply"
handleCApply' _ _ (CVar _ _)                 = error "handleApply'"
handleCApply' _ _ (CLit _ _)                 = error "handleApply'"
handleCApply' _ _ (CLambda _ _ _)            = error "handleApply'"
handleCApply' _ _ (CLetDecl _ _ _)           = error "handleApply'"
handleCApply' _ _ (CDoExpr _ _)              = error "handleApply'"
handleCApply' _ _ (CListComp _ _ _)          = error "handleApply'"
handleCApply' _ _ (CCase _ _ _ _)            = error "handleApply'"
handleCApply' _ _ (CTyped _ _ _)             = error "handleApply'"
handleCApply' _ _ (CRecConstr _ _ _)         = error "handleApply'"
handleCApply' _ _ (CRecUpdate _ _ _)         = error "handleApply'"     

                                                   
dummyBoxes:: Int -> Span -> (Int, Span, BoxLabel)
dummyBoxes id p = (id, p , mkExpBox False) 
                                                  
 
buildCApply:: CExpr -> [Span] -> [CExpr] -> CExpr
buildCApply sym  pos params@(_:_) = 
  buildCApply (CApply (head pos) sym (head params)) 
              (tail pos) 
              (tail params)
buildCApply sym  _             [] = sym

handleCApply:: Opts -> Bool -> CExpr -> INS CExpr 
handleCApply o isAlt (CApply pos exp1 exp2) = 
  instrumentWithExp isAlt pos (liftS2 (CApply pos) 
                                      (transExp o False exp1) 
                                      ((transExp o False exp2) 
                              `bindS` (applyBoolTick exp1 posi)))
  where posi = getPosExp exp2
handleCApply _ _ (CSymbol _ _)                 = error "handleApply"
handleCApply _ _ (CVar _ _)                 = error "handleApply"
handleCApply _ _ (CLit _ _)                 = error "handleApply"
handleCApply _ _ (CLambda _ _ _)            = error "handleApply"
handleCApply _ _ (CLetDecl _ _ _)           = error "handleApply"
handleCApply _ _ (CDoExpr _ _)              = error "handleApply"
handleCApply _ _ (CListComp _ _ _)          = error "handleApply"
handleCApply _ _ (CCase _ _ _ _)            = error "handleApply"
handleCApply _ _ (CTyped _ _ _)             = error "handleApply"
handleCApply _ _ (CRecConstr _ _ _)         = error "handleApply"
handleCApply _ _ (CRecUpdate _ _ _)         = error "handleApply"     


-- handleNoParams:: [CEpxr] -> [Spans] -> INS (CExpr) -> INS (CExpr)
-- handleNoParams p:params pos s =  
flatApply :: CExpr -> [(Span,CExpr)] -> [(Span,CExpr)]
flatApply (CApply pos exp1 exp2) coll  = flatApply exp1 ((pos,exp2):coll)
flatApply s@(CSymbol pos _)      coll  = (pos,s):coll
flatApply (CVar _ _) _                 = []
flatApply (CLit _ _) _                 = []
flatApply (CLambda _ _ _) _            = []
flatApply (CLetDecl _ _ _) _           = []
flatApply (CDoExpr _ _) _              = []
flatApply (CListComp _ _ _) _          = []
flatApply (CCase _ _ _ _) _            = []
flatApply (CTyped _ _ _) _             = [] 
flatApply (CRecConstr _ _ _) _         = [] 
flatApply (CRecUpdate _ _ _) _         = []  


------------------------------------------------------------------------------

extractITE :: CExpr -> Maybe (CExpr, CExpr, CExpr)
extractITE e = 
  case e of
     CApply _ (CApply _ (CApply _ (CSymbol _ (_,"Prelude","if_then_else"))
                         cond)
              tExp)
           fExp -> Just (cond, tExp, fExp)
     _          -> Nothing 

isITE:: CExpr -> Bool
isITE cexp = isJust $ extractITE cexp





