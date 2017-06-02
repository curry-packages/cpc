module CPC.Files
  (cpcSubdir, inCpcSubdir, addCpcSubdir, addInstSuffix, hasInstSuffix )
 where
  
import FilePath ((</>), splitFileName )  
import List     (isSuffixOf)
  
--- Rename a program into an instrumented program:
addInstSuffix :: String -> String
addInstSuffix mn = mn ++ "_Instrumented"

--- Is a program name the name of an instrumented program?
hasInstSuffix :: String -> Bool
hasInstSuffix mn = "_Instrumented" `isSuffixOf` mn

cpcSubdir :: String
cpcSubdir = ".cpc"

inCpcSubdir :: String -> String
inCpcSubdir fn = let (path, file) = splitFileName fn
                 in addCpcSubdir path </> file
                   
addCpcSubdir :: String -> String
addCpcSubdir dir = dir </> cpcSubdir