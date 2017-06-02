module CData
 where

import CMeta (CMeta, BoxLabel, readMeta', metaName, boxes, label, idBox, position)
import CTrack (eticks)
import qualified FiniteMap as FMA (FM, emptyFM, addToFM_C)
import ACSpans.AbstractCurrySpan
import qualified FMInt as FMI (FMI,elemFM)
import qualified ACSpans.Span as ACS (Span)
import ReadShowTerm (readUnqualifiedTerm)
import FilePath (replaceExtension)
import CPC.Files



mkMap :: String -> String -> IO (FMA.FM ACS.Span [BoxLabel])
mkMap metaPath trackPath = 
  do   track <- readFile $ inCpcSubdir $ replaceExtension trackPath "ctrack"
       let trackData = readUnqualifiedTerm ["FMInt","CTrack","Prelude"] track
       meta <-  readMeta' metaPath
       return ((consMap (boxes meta) (eticks trackData) 
                                      (FMA.emptyFM (<))))

consMap:: [(Int,((Int, Int), (Int, Int)),CMeta.BoxLabel)] ->
                                                  FMI.FMI ->
           FMA.FM ((Int, Int),(Int, Int))[CMeta.BoxLabel] ->
         FMA.FM ((Int, Int), (Int, Int)) [CMeta.BoxLabel]

consMap (b:boxes) track fm = let id = idBox b
                             in if (FMI.elemFM id track)
                                    then consMap boxes track 
                                         (FMA.addToFM_C (++) fm 
                                                        (position b) [label b])
                                    else consMap boxes track fm
consMap [] _ fm = fm
                     
  
