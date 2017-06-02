module CPC.Transform (transMain) where


import qualified ACSpans.AbstractCurrySpan as ACS
import ACSpans.Select (resultType, isIOType)
import Maybe


vpos:: ((Int,Int),(Int,Int))
vpos = ((0,0),(0,0)) 

transMain :: ACS.CurryProg -> String -> ACS.CurryProg
transMain prog name = transProg prog name


transProg :: ACS.CurryProg -> String -> ACS.CurryProg
transProg (ACS.CurryProg pos mname mnames tdecls fdecls odecls) nm = 
  addMain (ACS.CurryProg pos mname mnames tdecls 
          (map (transMainFunc nm) fdecls) odecls) nm 
          (isIOType $ resultTypeMain fdecls)

transMainFunc ::  String -> ACS.CFuncDecl -> ACS.CFuncDecl
transMainFunc  _ f@(ACS.CFunc pos (p, qname,fname) ar vis te rules)    = 
  if fname == "main" 
     then (ACS.CFunc pos (p, qname,"cpc_main") ar vis te rules) 
     else f
transMainFunc  _ f@(ACS.CmtFunc pos s (p, qname,fname) ar vis te rules)= 
  if fname == "main" 
     then (ACS.CmtFunc pos s (p, qname,"cpc_main") ar vis te rules)
     else f
    
addMain :: ACS.CurryProg -> String -> Bool -> ACS.CurryProg
addMain (ACS.CurryProg pos mname mnames tdecls fdecls odecls) nm te = 
  (ACS.CurryProg pos mname mnames tdecls fdecls' odecls)
  where fdecls' = (ACS.CFunc (Just vpos) (vpos, "","main") 0 ACS.Public 
                  ((ACS.CTCons vpos (vpos, "Prelude","IO") 
                  [ACS.CTCons vpos (vpos, "Prelude","()") []])) 
                  [ACS.CRule vpos [] 
                  (ACS.CSimpleRhs vpos 
                   (ACS.CApply vpos 
                    (ACS.CApply vpos 
                    (ACS.CSymbol vpos (vpos, "CLib", runName)) 
                    (ACS.CLit vpos (ACS.CStringc vpos nm))) 
                  (ACS.CSymbol vpos (vpos, "","cpc_main"))) 
                   [])]):fdecls
        runName = if te then "runIO" else "run"
       
resultTypeMain ::  [ACS.CFuncDecl] -> ACS.CTypeExpr
resultTypeMain  ((ACS.CFunc _ (_, _,fname) _ _ te _):fs)     = 
  if fname == "main" then resultType te else resultTypeMain fs 
resultTypeMain  ((ACS.CmtFunc _ _ (_, _,fname) _ _ te _):fs) = 
  if fname == "main" then resultType te else resultTypeMain fs
resultTypeMain                                   [] = error "No main function."
