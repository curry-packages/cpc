--------------------------------------------------------------------------------
--- This module provides an implementation of the state monad
--- @author fre, bjp
--- @version August 2013
--------------------------------------------------------------------------------

module INS
  ( INS
  , bindS
  , bindS_
  , returnS
  , getS
  , putS
  , modifyS
  , sequenceS
  , sequenceS_
  , mapS
  , mapS_
  , runState
  , evalState
  , execState
  , liftS
  , liftS2
  , updateID
  , updateID_
  ) where

import CMeta (BoxEntry,
                 BoxLabel,
                 mkExpBox,
                 mkTopBox,
                 mkifTBox,
                 mkifFBox,
                 mkLocBox)
                 
import ACSpans.AbstractCurrySpan
import Maybe
                 
infixl 1 `bindS`, `bindS_`



type MetaInfo = (Int, [BoxEntry])

type INS a = State MetaInfo a

type State s a = s -> (a, s)

bindS :: State s a -> (a -> State s b) -> State s b
bindS state f s = case state s of
                    (x, newS) -> newS `seq` f x newS

bindS_ :: State s a -> State s b -> State s b
bindS_ a b = a `bindS` \_ -> b

returnS :: a -> State s a
returnS x s = (x, s)

getS :: State s s
getS s = (s, s)

putS :: s -> State s ()
putS newState _ = ((), newState)

modifyS :: (s -> s) -> State s ()
modifyS f s = ((), f s)

--modifyS_f:: (s -> s) -> State s ()
--modifyS_f f (s1,s2) = ((),(f s1, s2))

--modifyS_s :: (s -> s) -> State s ()
--modifyS_s f (s1,s2) = ((),(s1, f s2))

sequenceS :: [State s a] -> State s [a]
sequenceS =
 foldr (\s newS -> s    `bindS` \a  ->
                   newS `bindS` \as ->
                   returnS (a:as))
       (returnS [])

sequenceS_ :: [State s a] -> State s ()
sequenceS_ = foldr bindS_ (returnS ())

mapS :: (a -> State s b) -> [a] -> State s [b]
mapS f = sequenceS . map f

mapS_ :: (a -> State s b) -> [a] -> State s ()
mapS_ f = sequenceS_ . map f

runState :: State s a -> s -> (a, s)
runState state s = state s

evalState :: State s a -> s -> a
evalState state s = fst (runState state s)

execState :: State s a -> s -> s
execState state s = snd (runState state s)

liftS :: (a -> b) -> State s a -> State s b
liftS f act = act `bindS` returnS . f

liftS2 :: (a -> b -> c) -> State s a -> State s b -> State s c
liftS2 f a b  = a `bindS` \x -> b `bindS` \y -> returnS (f x y)


updateID :: Maybe ((Int, Int), (Int, Int)) 
            -> (((Int, Int),(Int, Int)),String, String) 
            -> Bool -> (Int, [(Int, ((Int, Int), (Int, Int)), CMeta.BoxLabel)]) 
            -> ((),(Int,[(Int, ((Int, Int), (Int, Int)), CMeta.BoxLabel)]))
updateID posi (_,qname,name) isTop = getS `bindS` \(id,boxes)-> putS (id+1, (id, (fromMaybe ((0,0),(0,0)) posi), mkBox [qname,name]):boxes)
  where mkBox = if isTop then mkTopBox else mkLocBox

updateID_ :: Bool -> (Int, [(Int, CMeta.BoxLabel)]) 
         -> (Int, (Int, [(Int, CMeta.BoxLabel)]))

updateID_ isAlt = getS `bindS` \(id,boxes)-> putS (id+1, (id, mkExpBox isAlt):boxes) `bindS_` returnS id

