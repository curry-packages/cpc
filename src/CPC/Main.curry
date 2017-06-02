module CPC.Main (main) where


import CMeta ( mkCMeta
             , metaCreate
             , BoxLabel(..)
             , metaName
             , readMeta'
             , getBoxes )
import IO ( hPutStr
          , stdout
          , stderr
          , hPutStrLn )
import FileGoodies (stripSuffix, fileSuffix)
import FilePath ( takeBaseName
                , takeDirectory
                , (<.>)
                , replaceBaseName
                , replaceExtension
                , (</>)
                , splitFileName )
import Directory ( getCurrentDirectory
                 , getDirectoryContents
                 , removeFile
                 , createDirectoryIfMissing
                 , doesFileExist 
                 , getHomeDirectory )
import Distribution ( callFrontendWithParams
                    , defaultParams
                    , stripCurrySuffix
                    , addCurrySubdir
                    , FrontendTarget(..)
                    , installDir) 
import CData                       (mkMap)
import CPC.Report                  (report)
import CPC.Files                   (cpcSubdir, inCpcSubdir, addInstSuffix
                                   , hasInstSuffix )
import CPC.Pretty                  (showDProg)
import CPC.Instrumentation         ( startTransform )
import CPC.Transform               (transMain)
import CTrack
import AnsiiToHtml                 (convertToHTML)
import ArityMap                    (buildArityMap)
import Unsafe                      (unsafePerformIO)
import ReadShowTerm                (readQTerm, readUnqualifiedTerm)
import System                      (exitWith, getArgs, system)
import ACSpans.ExtendAbstractCurry (eProg)
import ACSpans.AddSpans            (apModule)
import ACSpans.SortSplit           (sortSplitModule)
import ACSpans.SpanUtils           (filterTS)
import AbstractCurry.Files         (readCurry)
import AbstractCurry.Select        (imports)
import IOExts                      (evalCmd)
import ACSpans.Pretty              (showCProg)
import ACSpans.Select              (progName, imports)
import ACSpans.AbstractCurrySpan   (CurryProg)
import ACSpans.Transform           (renameCurryModule)
import qualified FiniteMap as FMA  (fmToList, lookupFM, FM, emptyFM)
import qualified FMInt as FMI      (fmToList)
import GetOpt
import AnsiCodes
import List                        (nub)

import CPC.PackageConfig           (packagePath)

-- The path for imported run-time libraries (CLib, CTrack,...)
cpcImportPath :: String
cpcImportPath = packagePath </> "src"

main:: IO ()
main = do
  (args, files) <- getArgs >>= parse
  putStrLn cpcBanner
  putStrLn $ texcol"Setted Flags: " ++ show args
  putStrLn $ texcol"Given Files: " ++ show (files)
  cpc args files
  
cpc:: [Flag] -> [String] -> IO()
cpc _ [] = putStrLn "Use cpc --help for instructions."
cpc args (file:files) = do 
  
  let paths = map ((</> cpcSubdir) . fst . splitFileName) files
  createDirectoryIfMissing False cpcSubdir
  mapIO_ (createDirectoryIfMissing False) paths
  
  let mainFiles = filter isMainFlag args        
      (mainFile, imps)  = case mainFiles of 
                           []          -> (file,files)
                           [Main name] -> ((stripCurrySuffix name),(file:files))
                           _           -> error "Error: CaseMain"  
  
  when (Clean `elem` args) $ clean' file
  arityMap <- if (Optim `elem` args) 
                then buildArityMap (mainFile:imps) 
                else return (FMA.emptyFM (<))
  print arityMap
  
  putStrLn $ texcol"Starting transformation for all given files..."
  (_, spanProg, insProg, lastI, newProgName, _, datas) 
    <- handlingTrans mainFile imps 0 True args arityMap
    
  let spanProgMain = if any isMainFlag args then [] else [spanProg] 
  (_, spanProgs) <- case imps of
                              []     -> return (lastI, spanProgMain)
                              (_:_)  -> handlingImports' (lastI, spanProgMain)
                                        args imps arityMap
  putStrLn $ texcolr "Transformations: Done."
 {-                                       
  putStrLn "Writing cpix..."
  writeFile (inCpcSubdir $ replaceExtension mainFile "cpix") (show lastId)-}
  
  putStrLn $ texcol "Reading cpcrc..."
  homeDir  <- getHomeDirectory
  putStrLn $ texcolr "Reading cpcrc: Done."
  
  putStrLn $ texcol ("Compile instrumented program: " ++ newProgName)
  (x,y,z) <- evalCmd (installDir </> "bin" </> "curry")  
               [ ":set", "path", cpcImportPath
               , ":load", replaceExtension newProgName "curry"
               , ":save", ":quit"] ""


  putStrLn y
  putStrLn z
  putStrLn $ texcolr "Compiling: Done."
  
  putStrLn $ texcol "Evaluate instrumented program..."
  let binProgName = "." </> stripCurrySuffix newProgName
  system binProgName
  removeFile binProgName
  putStrLn $ texcolr "Evaluation: Done."
  
  putStrLn $ texcol "Calculating desired options..."
  when (TReport `elem` args) $ mapIO_ (report mainFile) (file:files)
  when (SpanProg `elem` args) $ mapIO_ print $ spanProgs
  when (Ticks `elem` args) $ print (FMI.fmToList $ 
                                    eticks (unsafePerformIO (readTrack $ 
                                                             getTrackName 
                                                             mainFile)))
  when (Evaluate `elem` args) $  putStrLn "In Work."
  when (MetaInfo `elem` args) $ mapIO_ 
                                (print .
                                 getBoxes . unsafePerformIO 
                                          . readMeta' . (metaName "")) 
                                (file:files) 
  when (Pretty `elem` args) $  mapIO_ (prettyPrint mainFile) $
                                      (reverse spanProgs)
  when (HTML `elem` args) $ mapIO_ (convertToHTML) 
                                   (map (ansiProg mainFile) 
                                        (reverse spanProgs))

  
isMainFlag :: Flag -> Bool
isMainFlag f = case f of 
                    Main _ -> True
                    _      -> False
 
prettyPrint:: String -> ACSpans.AbstractCurrySpan.CurryProg -> IO()
prettyPrint file spanProg = let (ansi,_) = ansiProg file spanProg
                            in  hPutStr stdout $ "\n" ++ ansi ++ "\n"
                            
ansiProg:: String -> ACSpans.AbstractCurrySpan.CurryProg -> (String,String)
ansiProg file spanProg = let cdatas = mkMap (progName spanProg) file
                             ansi   = showDProg (unsafePerformIO cdatas) 
                                                 spanProg
                         in (ansi,(progName spanProg))

handlingTrans :: String -> [String] -> Int -> Bool -> [Flag] 
              -> FMA.FM (String, String) Int 
              -> IO ([String]
                    , ACSpans.AbstractCurrySpan.CurryProg
                    , ACSpans.AbstractCurrySpan.CurryProg
                    , Int, String, String
                    , IO (FMA.FM ((Prelude.Int, Prelude.Int)
                                 ,(Prelude.Int, Prelude.Int))
                                 [CMeta.BoxLabel]))

handlingTrans file files offset rename args arMap = do       
  
         callFrontendWithParams TOKS defaultParams file
         callFrontendWithParams CY defaultParams file
         callFrontendWithParams ACY defaultParams file

         tokStr          <- readFile $ inCurrySubdir  
                                     $ replaceExtension file "tokens"
         astStr          <- readFile $ inCurrySubdir 
                                     $ replaceExtension file "cy"
         prog            <- readCurry file
        
         let toks     = filterTS (readQTerm tokStr)
             ast      = sortSplitModule $ 
                          readUnqualifiedTerm ["Prelude", "ACSpans.AST"] astStr
             spanAST  = fst $ apModule ast toks
             spanProg = eProg prog spanAST
             impChoice= case files of
                             [] -> []
                             xs -> map (repl . stripCurrySuffix) xs -- robust
             (insMain,(lastId,boxes)) = startTransform offset spanProg 
                                                       impChoice rename 
                                                       ((any isMainFlag args)
                                                         && offset==0) 
                                                       arMap
             insProg = if offset == 0 then transMain insMain file else insMain
             newProg = showCProg $ if rename
                                     then renameCurryModule 
                                            (addInstSuffix (progName insProg)) 
                                            insProg 
                                     else insProg
             cmeta = mkCMeta boxes
             datas = mkMap file file
             newProgName =  replaceBaseName file
                              (addInstSuffix (takeBaseName file)) 
             repl s      = map (\c -> if c == '/' then '.' else c) s
         
         putStrLn $ texcol ("Writing " ++ newProgName ++ ".curry...")
         writeFile (replaceExtension newProgName "curry") newProg
         putStrLn $ texcolr("Writing " ++ newProgName ++ ".curry: Done.") 
         
         putStrLn $ texcol("Writing " ++ file ++ ".cmeta...")
         metaCreate file cmeta
         putStrLn $ texcolr("Writing " ++ file ++ ".cmeta: Done.")
         
         return (impChoice,spanProg,insProg
                ,lastId,newProgName, newProg, datas)        
         
handlingImports':: (Int, [CurryProg]) -> [Flag] -> [String] 
                -> (FMA.FM (String,String) Int) -> IO (Int, [CurryProg])  
handlingImports' (offset, spanProgs) args files arM = 
  do -- files
    let choice  = case files of 
                       []     -> []
                       (x:xs) -> (x:xs)
    handlingImports (offset, spanProgs) choice args arM
   
handlingImports:: (Int, [CurryProg]) -> [String] -> [Flag] 
               -> (FMA.FM (String,String) Int) -> IO (Int, [CurryProg])  
handlingImports (offset, spanProgs) (imp:imprts) args arM = 
  do                     
    (_,spanProg,_,newId,_,_,_) <- handlingTrans imp [] offset 
                                                True args arM
    handlingImports (newId, spanProg:spanProgs) imprts args arM
handlingImports (offset, spanProgs) [] _ _ = 
  do 
    return (offset, spanProgs)
                              
                       
data Flag = 
     TReport               -- -t
   | Evaluate              -- -e
   | SpanProg              -- -s
   | Ticks                 -- -x
   | MetaInfo              -- -m
   | Main String           -- --main
   | Pretty                -- -p
   | Optim                 -- -o
   | Clean                 -- -c
   | HTML                  -- -h
   | Help                  -- -help



flags :: [GetOpt.OptDescr Flag]      
flags =
       [Option ['t'] []       (NoArg TReport)
            "Print a statistical report of the given files."
       ,Option ['s'] []       (NoArg SpanProg)
            "Print the calculated files with span information."
       ,Option ['e'] []       (NoArg Evaluate)
            "Run the instrumented code and collects ticks data."
       ,Option ['x'] []       (NoArg Ticks)
            "Print the collected ticks."
       ,Option ['m'] []       (NoArg MetaInfo)
           "Print the collected meta information."
       ,Option ['p'] []       (NoArg Pretty)
           "Pretty print the files with colored ticks."
       ,Option ['o'] []       (NoArg Optim)
           "Optimize ticks."
       ,Option ['h'] []       (NoArg HTML)
           "Build a HTML colored output."
       ,Option ['c'] []       (NoArg Clean)
           "Clean old data files before execution."
       ,Option [] ["main"]    (ReqArg Main "MAIN")
           "Do not instrument module MAIN for program coverage."
       ,Option []    ["help"] (NoArg Help)
           "Print this help message"
       ]
       
inCurrySubdir :: String -> String
inCurrySubdir fn = let (path, file) = splitFileName fn
                   in addCurrySubdir path </> file

parse:: [String] -> IO ([Flag], [String])                   
parse argv = case getOpt Permute flags argv of
        (args,fs,[]) -> do
            let files = if null fs then ["-"] else fs
            if Help `elem` args
              then do putStrLn cpcBanner
                      hPutStrLn stderr (usageInfo header flags)
                      exitWith 0
              else return (nub args, files)
 
        (_,_,errs)   -> do
            hPutStrLn stderr (concat errs ++ usageInfo header flags)
            exitWith 1
 where header = "Usage: cpc -option [modules]"
      
clean :: String -> IO ()
clean dir = do
  contents <- getDirectoryContents dir
  sequenceIO_ (map removeFile [dir++file| file <- contents
                                        , (fileSuffix file == "ctrack")
                                       || (fileSuffix file == "cmeta") ])
  sequenceIO_ (map removeFile [dir++file| file <- contents
                                        , hasInstSuffix (takeBaseName file)])
  putStrLn "Cleaned."
  
clean' :: String -> IO ()
clean' file = do
  contents <- getDirectoryContents ".cpc"
  mapIO_ removeFile [ ".cpc/"++fil | fil <- contents
                                   , takeBaseName fil == file ]
  return ()

-------------------------------------------------------------------------------
texcol:: String -> String
texcol = blue

texcolr:: String -> String
texcolr = green

cpcBanner:: String
cpcBanner =
  texcol "  ___   ___    ___ \n" ++
  texcol " /  _) |   \\  /  _)\n" ++
  texcol "(  (_  | __/ (  (_ \n" ++
  texcol " \\___) |_|    \\___)\n" 
