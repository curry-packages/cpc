module ACSpans.Span where

-- |Position
type Pos = (Int, Int)

-- |Span (start and end position)
type Span = (Pos, Pos)

-- |start position of a span
start :: Span -> Pos
start = fst

-- |end position of a span
end :: Span -> Pos
end = snd

startPos :: Pos
startPos = (1,1)

-- |Virtual position for AST-elements that have no "physical" positions like
--  e.g. the import of Prelude
virtualPos :: Pos
virtualPos = (0, 0)

-- |Virtual span for AST-elements that have no "physical" spans like
--  e.g. the import of Prelude
virtualSpan :: Span
virtualSpan = (virtualPos, virtualPos)

-- |Is a position a virtual position?
isVirtualPos :: Pos -> Bool
isVirtualPos p = p == virtualPos
