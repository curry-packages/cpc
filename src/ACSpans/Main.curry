module ACSpans.Main where

import AbstractCurry.Files (readCurry)
import Distribution
import FilePath (replaceExtension, (</>), splitFileName)
import ReadShowTerm (readQTerm, readUnqualifiedTerm)

import ACSpans.ExtendAbstractCurry (eProg)
import ACSpans.AddSpans            (apModule)
import ACSpans.SortSplit           (sortSplitModule)
import ACSpans.SpanUtils           (filterTS)
import ACSpans.Token
-- import ACSpans.Transform (renameCurryModule)
import ACSpans.Pretty              (showCProg)

main :: String -> IO ()
main modpath = do
  callFrontendWithParams TOKS defaultParams modpath
  callFrontendWithParams CY defaultParams modpath
  callFrontendWithParams ACY defaultParams modpath
  tokStr <- readFile $ inCurrySubdir $ replaceExtension modpath "tokens"
  astStr <- readFile $ inCurrySubdir $ replaceExtension modpath "cy"
  prog   <- readCurry modpath
  let toks     = filterTS (readQTerm tokStr)
      ast      = sortSplitModule $ readUnqualifiedTerm ["Prelude", "ACSpans.AST"] astStr
      spanAST  = fst $ apModule ast toks
      spanProg = eProg prog spanAST

  print spanProg

inCurrySubdir :: String -> String
inCurrySubdir fn = let (path, file) = splitFileName fn
                   in addCurrySubdir path </> file
