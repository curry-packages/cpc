module CMeta (CMeta,
                  CpcPos,
                  BoxEntry,
                  BoxLabel(..),
                  CondBox,
                  mkExpBox,
                  metaCreate,
                  readMeta',
                  metaName,
                  getBoxes,
                  mkTopBox,
                  mkLocBox,
                  mkifTBox,
                  mkifFBox,
                  mkGuardTBox,
                  mkGuardFBox,
                  position, label, idBox,
                  isExp,
                  isAlt,
                  isCond,
                  isBin,
                  boxes,
                  mkCMeta) where

import CTrack

import ReadShowTerm (readQTermFile)
import FilePath
import CPC.Files


-- Tab stops are the size of a tab in the provided /line:column/ values.

data CMeta = CMeta
       --      FilePath           -- location of original file
       --      CalendarTime            -- time of original file's last update
       --      Int                -- tab stop value.
             [BoxEntry]         -- entries

type CpcPos = (Int, Int)             
             
type BoxEntry = (Int, (CpcPos,CpcPos), BoxLabel)

data BoxLabel = ExpBox  Bool -- isAlt
              | TopLevelBox [String]
              | LocalBox [String]
              | BinBox CondBox Bool

data CondBox = GuardBinBox
             | CondBinBox
             | QualBinBox
             
mkExpBox :: Bool -> BoxLabel
mkExpBox bool = ExpBox bool

mkTopBox :: [String] -> BoxLabel
mkTopBox strings = TopLevelBox strings

mkLocBox :: [String] -> BoxLabel
mkLocBox strings = LocalBox strings

mkifTBox :: BoxLabel
mkifTBox = BinBox CondBinBox True 

mkifFBox :: BoxLabel
mkifFBox = BinBox CondBinBox False

mkGuardTBox :: BoxLabel
mkGuardTBox = BinBox GuardBinBox True 

mkGuardFBox :: BoxLabel
mkGuardFBox = BinBox GuardBinBox False

metaCreate :: String -> CMeta -> IO ()
metaCreate modname cmeta = 
  writeFile (inCpcSubdir $ replaceExtension modname "cmeta") (show cmeta)

mkCMeta :: [BoxEntry] -> CMeta
mkCMeta boxes' = CMeta boxes'

idBox :: BoxEntry -> Int
idBox (id, _, _) = id

position :: BoxEntry -> (CpcPos,CpcPos)
position (_,pos,_) = pos

label :: BoxEntry -> BoxLabel
label (_,_,label') = label'

boxes :: CMeta -> [BoxEntry]
boxes (CMeta boxes') = boxes'
 
readMeta' :: String -> IO CMeta
readMeta' filename = 
  do 
  -- print "Reading .cmeta..."
    contents <- readQTermFile $ inCpcSubdir $ replaceExtension filename "cmeta"
    return contents 

metaName :: FilePath -> String -> String
metaName dirName name = dirName </> name <.> "cmeta"

getBoxes :: CMeta -> [BoxEntry]
getBoxes (CMeta b) = b

isExp:: BoxEntry -> Bool
isExp box = case box of
                  (_, _, ExpBox _) -> True
                  (_, _, _) -> False
                  
isAlt:: BoxEntry -> Bool
isAlt box = case box of
                  (_, _, ExpBox True) -> True
                  _ -> False
 
isBin:: BoxEntry -> Bool
isBin box = case box of
                  (_, _,  BinBox _ _) -> True
                  _ -> False 
                  
isCond:: BoxLabel -> Bool
isCond box = case box of
                  (BinBox CondBinBox _) -> True
                  _ -> False
              
              