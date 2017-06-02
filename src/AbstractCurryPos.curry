module AbstractCurryPos where

import AbstractCurry.Types (pre,version)

--- Position
type Pos = (Int, Int) -- type for only one position
type PosD = (Pos,Pos) -- type for start and end positions

--- A module name.
type MName = String

--- The data type for representing qualified names.
type QName = (MName, String)

--- Data type to specify the visibility of various entities.
data CVisibility
  = Public    
  | Private  

--- Data type for representing a Curry module in the intermediate form.
data CurryProg = CurryProg PosD MName [MName] [CTypeDecl] [CFuncDecl] [COpDecl]

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
data CTypeDecl
  = CType    QName CVisibility [CTVarIName] [CConsDecl]
  | CTypeSyn QName CVisibility [CTVarIName] CTypeExpr
  | CNewType QName CVisibility [CTVarIName] CConsDecl

--- The type for representing type variables.
type CTVarIName = (Int, String)

--- A constructor
data CConsDecl
  = CCons   QName CVisibility [CTypeExpr]
  | CRecord QName CVisibility [CFieldDecl]

--- A record field declaration 
data CFieldDecl = CField QName CVisibility CTypeExpr

--- Type expression.
data CTypeExpr
  = CTVar CTVarIName               
  | CFuncType CTypeExpr CTypeExpr  
  | CTCons QName [CTypeExpr]       
                                  

--- Labeled record fields
type CField a = (QName, a)

--- Data type for operator declarations.
data COpDecl = COp QName CFixity Int

--- Data type for operator associativity
data CFixity
  = CInfixOp  
  | CInfixlOp 
  | CInfixrOp  

--- Function arity
type Arity = Int

--- Data type for representing function declarations.
data CFuncDecl                                                        -- must
  = CFunc          PosD QName Arity CVisibility CTypeExpr [CRule] 
  | CmtFunc String PosD QName Arity CVisibility CTypeExpr [CRule]

--- The general form of a function rule
data CRule = CRule PosD [CPattern] CRhs -- nice to have

--- Right-hand-side of a 'CRule' or a `case` expression.
data CRhs
  = CSimpleRhs  PosD CExpr            [CLocalDecl] -- nice to have
  | CGuardedRhs PosD [(CExpr, CExpr)] [CLocalDecl] -- nice to have

--- Data type for representing local (let/where) declarations
data CLocalDecl
  = CLocalFunc CFuncDecl     
  | CLocalPat  CPattern CRhs 
  | CLocalVars [CVarIName]  

--- Data types for representing object variables.
type CVarIName = (Int,String)

--- Data type for representing pattern expressions.
data CPattern
  = CPVar      CVarIName               
  | CPLit      CLiteral             
  | CPComb     QName [CPattern]        
                                       
  | CPAs       CVarIName CPattern   
  | CPFuncComb QName [CPattern]      
  | CPLazy     CPattern                
  | CPRecord   QName [CField CPattern] 

--- Data type for representing Curry expressions.
data CExpr                                              -- must
 = CVar       PosD CVarIName                        
 | CLit       PosD CLiteral                         
 | CSymbol    PosD QName                            
 | CApply     PosD CExpr CExpr                     
 | CLambda    PosD [CPattern] CExpr               
 | CLetDecl   PosD [CLocalDecl] CExpr               
 | CDoExpr    PosD [CStatement]                     
 | CListComp  PosD CExpr [CStatement]               
 | CCase      PosD CCaseType CExpr [(CPattern, CRhs)] 
 | CTyped     PosD CExpr CTypeExpr                   
 | CRecConstr PosD QName [CField CExpr]              
 | CRecUpdate PosD CExpr [CField CExpr]            

--- Data type for representing literals occurring in an expression.
data CLiteral
  = CIntc   Int
  | CFloatc Float
  | CCharc  Char
  | CStringc String

--- Data type for representing statements in do expressions and
--- list comprehensions.
data CStatement                                         -- must
  = CSExpr PosD CExpr        
  | CSPat PosD CPattern CExpr
  | CSLet PosD [CLocalDecl]   

--- Type of case expressions
data CCaseType
  = CRigid
  | CFlex  

