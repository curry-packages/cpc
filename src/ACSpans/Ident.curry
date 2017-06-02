module ACSpans.Ident where

import ACSpans.Span (Span, virtualSpan)

-- |Simple identifier
data Ident = Ident
  { idSpan   :: Span     -- ^ Source code 'Span'
  , idName   :: String   -- ^ Name of the identifier
  , idUnique :: Int      -- ^ Unique number of the identifier
  }

-- |Qualified identifier
data QualIdent = QualIdent
  { qidModule :: Maybe ModuleIdent -- ^ optional module identifier
  , qidIdent  :: Ident             -- ^ identifier itself
  }

-- | Module identifier
data ModuleIdent = ModuleIdent
  { midSpan       :: Span     -- ^ source code 'Span'
  , midQualifiers :: [String] -- ^ hierarchical idenfiers
  }

-- |QualIdent that might have some kind of surrounding symbols,
-- |e.g. parens or backticks
-- 1. (Maybe Span) -  opening symbol like LeftParen or Backtick
-- 2. (Maybe Span) -  closing symbol like RightParen or Backtick
data SymIdent = SymIdent (Maybe Span) Ident (Maybe Span)

data SymQualIdent = SymQualIdent (Maybe Span) QualIdent (Maybe Span)

-- |Global scope for renaming
globalScope :: Int
globalScope = 0

-- |Construct an 'Ident' from a 'String'
mkIdent :: String -> Ident
mkIdent x = Ident virtualSpan x globalScope

mkSQIdent :: QualIdent -> SymQualIdent
mkSQIdent qi = SymQualIdent Nothing qi Nothing

-- | Convert an 'Ident' to a 'QualIdent'
qualify :: Ident -> QualIdent
qualify = QualIdent Nothing

-- | Convert an 'Ident' to a 'QualIdent' with a given 'ModuleIdent'
qualifyWith :: ModuleIdent -> Ident -> QualIdent
qualifyWith = QualIdent . Just

-- | Construct an 'Ident' for an n-ary tuple where n > 1
tupleId :: Int -> Ident
tupleId n
  | n > 1     = mkIdent $ '(' : replicate (n - 1) ',' ++ ")"
  | otherwise = error $ "ExtendAbstractCurry.tupleId: wrong arity " ++ show n

-- | 'Ident' for the value '[]'
nilId :: Ident
nilId = mkIdent "[]"

-- | 'Ident' for the function ':'
consId :: Ident
consId = mkIdent ":"

-- | 'Ident' for the type/value unit ('()')
unitId :: Ident
unitId = mkIdent "()"

-- | 'Ident' for the type '[]'
listId :: Ident
listId = mkIdent "[]"

-- | 'QualIdent' for the type/value unit ('()')
qUnitId :: QualIdent
qUnitId = qualify unitId

-- | 'QualIdent' for the type '[]'
qListId :: QualIdent
qListId = qualify listId

-- | 'QualIdent' for the type of n-ary tuples
qTupleId :: Int -> QualIdent
qTupleId = qualify . tupleId

-- | 'QualIdent' for the constructor '[]'
qNilId :: QualIdent
qNilId = qualify nilId

-- | 'QualIdent' for the constructor ':'
qConsId :: QualIdent
qConsId = qualify consId

qEnumFromId :: QualIdent
qEnumFromId = qualifyWith preludeMIdent (mkIdent "enumFrom")

qEnumFromThenId :: QualIdent
qEnumFromThenId = qualifyWith preludeMIdent (mkIdent "enumFromThen")

qEnumFromToId :: QualIdent
qEnumFromToId = qualifyWith preludeMIdent (mkIdent "enumFromTo")

qEnumFromThenToId :: QualIdent
qEnumFromThenToId = qualifyWith preludeMIdent (mkIdent "enumFromThenTo")

qNegateId :: QualIdent
qNegateId = qualifyWith preludeMIdent (mkIdent "negate")

qFlip :: QualIdent
qFlip = qualifyWith preludeMIdent (mkIdent "flip")

qIfThenElseId :: QualIdent
qIfThenElseId = qualifyWith preludeMIdent (mkIdent "if_then_else")

-- | 'ModuleIdent' for the Prelude
preludeMIdent :: ModuleIdent
preludeMIdent = ModuleIdent virtualSpan ["Prelude"]
