module ArityMap where

import FiniteMap
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Types
import Distribution (installDir)
import List (nub,partition)
import Directory (doesFileExist)
import Unsafe (unsafePerformIO)
import FilePath ((<.>))

buildArityMap:: [String] -> IO (FM QName Int) 
buildArityMap files = 
  do 
     --print files
     acfuncs <- mapIO readCurry ("Prelude":files)
     -- print ("acfuncs: " ++ (show acfuncs))
     let imps = (concatMap imports acfuncs)
         (selfs,libs) = partition ( unsafePerformIO 
                                  . doesFileExist 
                                  . (<.> "curry"))
                                  imps
     --   libs2 = map (path++) libs
     --print ("Imps: " ++ (show  imps))
     
     impFuncs <- mapIO readCurry (selfs++libs)
     --print imps
     --print ("Selfs: " ++ (show selfs))
     --print ("Libs: " ++ (show libs))
     let totalFuncs = impFuncs++acfuncs
     let acArities = map extractArity (concatMap functions totalFuncs)
     --print ("acarities: " ++ (show acArities))
     return $ (addListToFM (emptyFM (<)) acArities)
 
     
extractArity:: CFuncDecl -> (QName, Arity)
extractArity (CFunc     qName ar _ _ _) = (qName,ar)
extractArity (CmtFunc _ qName ar _ _ _) = (qName,ar)

path:: String
path = installDir++"/lib/"

