module CLib (tick, boolTick,
                   run, runIO,
                   offset, tickRef) where

import CTrack (Track, writeTrack)
import CMeta  (CMeta)

import Unsafe (unsafePerformIO)
import System (getProgName)
import FilePath
import Global
import FMInt
import Read (readInt)
import CPC.Files
import AllSolutions(getAllValues)
import Debug


tickRef :: Global (FMI)
tickRef = global (emptyFM) Temporary

tick :: Int -> a -> a
tick id prog = unsafePerformIO $ do
   track <- readGlobal tickRef
   let v = lookupWithDefaultFM track 0 id
   writeGlobal tickRef (addToFM track id (v+1))
   --print (fmToList track)
   return prog  

boolTick :: Int -> Int -> Bool -> Bool
boolTick idt ide prog = case prog of
                             True -> tick idt $! prog
                             False -> tick ide $! prog

  {-
   track <- readGlobal tickRef
   let id = if prog then idt else ide
       v  = lookupWithDefaultFM track 0 id
   writeGlobal tickRef (addToFM track id (v+1))

   return prog  -}


run :: String -> a -> IO ()
run name mainexp = do
  getAllValues mainexp >>= mapIO_ print
  finalize name
                 
runIO :: String -> IO a -> IO ()        
runIO name mainexp = mainexp >> finalize name

finalize:: String -> IO ()
finalize name = do      
  track <- readGlobal tickRef
  finTrack name track
 where
  finTrack progName track = writeTrack progName track 0


--TODO
offset::Int
offset = 0
