-- | Datatypes and file-access routines for the tracking data file
module CTrack (Track,
               TrackInfo,
               trackName,
               trackTicks,
               setTicks,
               trackInfo,
               readTrack,
               writeTrack,
               getTrackFilename,
               getTrackName,
               extractIOTrack,
               eticks,
               readTrack',
               --initializeTrack,
               initTrack) where

import FilePath
import ReadShowTerm (readQTermFile)
import Unsafe (unsafePerformIO)
import CPC.Files
import qualified FMInt as FMI
import Directory (doesFileExist)
import AnsiCodes

data Track = Track TrackInfo

data TrackInfo = TrackInfo
                 String    --  module name
                 Int       --
                 (FMI.FMI) --  ticks of expressions
                 
---------------------------------------------------------------------------

trackName :: TrackInfo -> String
trackName (TrackInfo name  _ _) = name

trackTicks :: TrackInfo  -> FMI.FMI
trackTicks (TrackInfo  _ _ ticks) = ticks

eticks :: Track -> FMI.FMI
eticks (Track (TrackInfo _ _ t)) = t

setTicks :: FMI.FMI -> Track -> Track
setTicks ticks (Track (TrackInfo s len _)) = Track (TrackInfo s len ticks)

trackInfo :: Track -> TrackInfo
trackInfo (Track info) = info

readTrack :: String -> IO Track
readTrack filename = 
 do 
   putStrLn $ blue ("Reading " ++ replaceExtension filename "ctrack" ++ "...")
   contents <- readQTermFile $ inCpcSubdir $ replaceExtension filename "ctrack"
   putStrLn $ 
        green ("Reading " ++ replaceExtension filename "ctrack" ++ ": Done.")
   return contents
                        
readTrack' :: String -> IO FMI.FMI
readTrack' filename = 
 do 
  putStrLn $ (blue "Reading " ++ replaceExtension filename "ctrack" ++ "...")
  contents <- readQTermFile $ inCpcSubdir $ replaceExtension filename "ctrack"
  let conTrack = eticks (contents)
  putStrLn $ 
      green ("Reading " ++ replaceExtension filename "ctrack" ++ ": Done.")
  return conTrack
  
upTrack:: Track -> FMI.FMI -> Track
upTrack (Track (TrackInfo s i fm)) newTicks = Track (TrackInfo s i nfm)
  where nfm = if FMI.isEmptyFM fm 
                then error "here" 
                else FMI.addListToFM fm (FMI.fmToList newTicks)
  
writeTrack :: String -> FMI.FMI -> Int -> IO ()
writeTrack name fm maxId        = 
 do    
   putStrLn
         $ blue ("Writing " ++ replaceExtension name "ctrack" ++ "...")
   writeFile file (show track)
   putStrLn 
         $ green ("Writing " ++ replaceExtension name "ctrack" ++ ": Done.")
  where file     = inCpcSubdir $ replaceExtension name "ctrack"
        track    = initTrack name maxId fm


getTrackFilename :: String -> String
getTrackFilename str = replaceExtension str ".ctrack"

getTrackName :: String -> String
getTrackName name = name <.> "ctrack"

extractIOTrack :: IO Track -> Track
extractIOTrack iotrack = unsafePerformIO iotrack

initTrack :: String -> Int -> FMI.FMI -> Track
initTrack name maxID trackList = Track (TrackInfo name maxID trackList)