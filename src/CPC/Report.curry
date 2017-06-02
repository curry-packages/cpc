module CPC.Report (report) where

import CMeta
import CTrack

import List
import Float
import IO (hPutStr,
           stdout)
import FMInt (keysFM,fmToList)

data AnalyzeData = ExpData Float Float | AltData Float Float | CondData Float Float

report :: String -> String -> IO ()
report name modName = do
              track <- readTrack name
              meta <- readMeta' modName
        --      print (fmToList $ eticks track)
              prettyPrintResults (analyzeTrack (keysFM $ eticks track) meta) modName

analyzeTrack :: [Int] -> CMeta -> [AnalyzeData]
analyzeTrack ticks cmeta = let boxes     = getBoxes cmeta
                               expBoxes  = filter (isExp)  boxes
                               altBoxes  = filter (isAlt)  boxes
                               condBoxes = filter (isBin) boxes
                               usedBoxes = [(x,y,z)| (x,y,z) <- expBoxes, x `elem` ticks]
                               usedAlts  = [(x,y,z)| (x,y,z) <- altBoxes, x `elem` ticks]   
                               usedConds = [(x,y,z)| (x,y,z) <- condBoxes, x `elem` ticks]
                            in [(ExpData  (i2f(length usedBoxes)) (i2f(length expBoxes))), 
                               (AltData  (i2f(length usedAlts))  (i2f(length altBoxes))),
                               (CondData (i2f(length usedConds)) (i2f(length condBoxes)))] 


prettyPrintResults :: [AnalyzeData] -> String -> IO ()
prettyPrintResults resultDatas modName = do 
                                hPutStr stdout ("\nResults for " ++ modName ++ "\n" ++ (ppResults "" resultDatas))

ppResults :: String -> [AnalyzeData] -> String
ppResults output (singleData:xs) = case singleData of
                                        (ExpData used sum)  -> ppResults (output ++ ("\nExpression coverage:   " ++ percString used sum ++ " expressions used.\n")) xs
                                        (AltData used sum)  -> ppResults (output ++ ("Alternatives coverage: " ++ percString used sum ++ " alternatives used.\n")) xs
                                        (CondData used sum) -> ppResults (output ++ ("Boolean coverage:      " ++ percString used sum ++ " bools used.\n")) xs
ppResults output [] = output
--++ show((div used sum)*100) ++ "% ," 

percString:: Float -> Float -> String
percString used sum | sum < 1   = "100% (0/0)"
                    | otherwise = show ((*.) ((/.) used sum) 100) ++ "% (" ++ show(used) ++ "/" ++ show(sum) ++ ")"