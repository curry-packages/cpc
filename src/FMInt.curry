-----------------------------------------------------------------------------
--- A finite map is an efficient purely functional data structure
--- to store a mapping from keys to values.
--- In order to store the mapping efficiently, an irreflexive(!) order predicate
--- has to be given, i.e., the order predicate `le` should not satisfy
--- `(le x x)` for some key `x`.
---
--- Example: To store a mapping from `Int -> String`, the finite map needs
--- a Boolean predicate like `(<)`.
--- This version was ported from a corresponding Haskell library
---
--- @author Frank Huch, Bernd Brassel
--- @version March 2013
--- @category algorithm
-----------------------------------------------------------------------------

module FMInt (
        FMI,                -- abstract type

        emptyFM,
        unitFM,
        listToFM,

        addToFM,
        addToFM_C,
        addListToFM,
        addListToFM_C,
        delFromFM,
        delListFromFM,
        splitFM,

        plusFM,
        plusFM_C,
        minusFM,
        intersectFM,
        intersectFM_C,

        foldFM,
--         mapFM,
--         filterFM,

        sizeFM,
        eqFM,
        isEmptyFM,
        elemFM,
        lookupFM,
        lookupWithDefaultFM,
        keyOrder,

        fmToList,
        keysFM,
        eltsFM,
--         fmSortBy,

        minFM,maxFM,updFM, fmToListPreOrder,

        showFM, readFM
    ) where

import Maybe
import ReadShowTerm (readQTerm, showQTerm)

--- order predicates are boolean
type LeKey = Int -> Int -> Bool

-----------------------------------------------
--        BUILDING finite maps
-----------------------------------------------

--- The empty finite map.
--- @param le an irreflexive order predicate on the keys.
--- @result an empty finite map
emptyFM :: FMI
emptyFM = FMI EmptyFMI

--- Construct a finite map with only a single element.
--- @param le an irreflexive order predicate on the keys.
--- @param key key of
--- @param elt the single element to form
--- @result a finite map with only a single element
unitFM :: Int -> Int -> FMI
unitFM key elt = FMI (unitFM' key elt)

unitFM' :: Int -> Int -> FiniteMap
unitFM' key elt = BranchFMI key elt 1 EmptyFMI EmptyFMI


--- Builts a finite map from given list of tuples (key,element).
--- For multiple occurences of key, the last corresponding
--- element of the list is taken.
--- @param le an irreflexive order predicate on the keys.
listToFM :: [(Int,Int)] ->  FMI 
listToFM = addListToFM (emptyFM)

-----------------------------------------------
--        ADDING AND DELETING
-----------------------------------------------

--- Throws away any previous binding and stores the new one given.

addToFM :: FMI -> Int -> Int  -> FMI 
addToFM (FMI fm) key elt = FMI (addToFM' fm key elt)

addToFM' :: FiniteMap -> Int -> Int -> FiniteMap
addToFM' fm key elt = addToFM_C' (\ _ new -> new) fm key elt

addToFM_C' :: (Int -> Int -> Int)
           -> FiniteMap -> Int -> Int -> FiniteMap
addToFM_C' _ EmptyFMI key elt = unitFM' key elt
addToFM_C' combiner (BranchFMI key elt size fm_l fm_r) new_key new_elt
  = if new_key < key
    then mkBalBranch key elt (addToFM_C' combiner fm_l new_key new_elt) fm_r
    else
      if new_key==key
      then BranchFMI new_key (combiner elt new_elt) size fm_l fm_r
      else mkBalBranch key elt fm_l (addToFM_C' combiner fm_r new_key new_elt)


--- Throws away any previous bindings and stores the new ones given.
--- The items are added starting with the first one in the list
addListToFM :: FMI -> [(Int,Int)] -> FMI
addListToFM (FMI fm) key_elt_pairs =
  FMI (addListToFM' fm key_elt_pairs)

addListToFM' :: FiniteMap
             -> [(Int, Int)] -> FiniteMap
addListToFM' fm key_elt_pairs =
  addListToFM_C' (\ _ new -> new) fm key_elt_pairs

addListToFM_C' :: (Int -> Int -> Int)
               -> FiniteMap -> [(Int, Int)] -> FiniteMap
addListToFM_C' combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs        -- foldl adds from the left
  where
    add fmap (key,elt) = addToFM_C' combiner fmap key elt


--- Instead of throwing away the old binding,
--- addToFM_C combines the new element with the old one.
--- @param combiner a function combining to elements
--- @param fm a finite map
--- @param key the key of the elements to be combined
--- @param elt the new element
--- @result a modified finite map
addToFM_C :: (Int -> Int -> Int) -> FMI -> Int -> Int
                                 -> FMI
addToFM_C combiner (FMI fm) key elt =
  FMI (addToFM_C' combiner fm key elt)

--- Combine with a list of tuples (key,element), cf. addToFM_C
addListToFM_C :: (Int -> Int -> Int) -> FMI -> [(Int,Int)]
                                      -> FMI 
addListToFM_C combiner (FMI fm) key_elt_pairs =
  FMI (addListToFM_C' combiner fm key_elt_pairs)

--- Deletes key from finite map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
delFromFM :: FMI -> Int   -> FMI 
delFromFM (FMI fm) del_key = FMI (delFromFM' fm del_key)

delFromFM' :: FiniteMap -> Int -> FiniteMap
delFromFM' EmptyFMI _ = EmptyFMI
delFromFM' (BranchFMI key elt _ fm_l fm_r) del_key
  = if del_key < key
    then mkBalBranch key elt (delFromFM' fm_l del_key) fm_r
    else
      if del_key==key
        then glueBal fm_l fm_r
        else mkBalBranch key elt fm_l (delFromFM' fm_r del_key)

--- Deletes a list of keys from finite map.
--- Deletion doesn't complain if you try to delete something
--- which isn't there
delListFromFM        :: FMI -> [Int] -> FMI
delListFromFM (FMI fm) keys = FMI (foldl (delFromFM') fm keys)

--- Applies a function to element bound to given key.
updFM :: FMI -> Int -> (Int -> Int) -> FMI
updFM (FMI fm) i f = FMI (upd fm)
  where
    upd EmptyFMI                          =  EmptyFMI
    upd (BranchFMI k x h l r)
            | i == k     =  BranchFMI k (f x) h l r
            | i < k     =  BranchFMI k x h (upd l) r
            | otherwise  =  BranchFMI k x h l (upd r)

--- Combines delFrom and lookup.
splitFM :: FMI -> Int -> Maybe (FMI,(Int,Int))
splitFM g v = maybe Nothing (\x->Just (delFromFM g v,(v,x))) (lookupFM g v)

-------------------------------------------------
-- COMBINING finite maps
-------------------------------------------------

--- Efficiently add key/element mappings of two maps into a single one.
--- Bindings in right argument shadow those in the left
plusFM :: FMI -> FMI -> FMI
plusFM (FMI fm1) (FMI fm2) = FMI (plusFM' fm1 fm2)

plusFM' :: FiniteMap -> FiniteMap -> FiniteMap
plusFM'  EmptyFMI fm2 = fm2
plusFM' (BranchFMI split_key1 elt1 s1 left1 right1) EmptyFMI =
  (BranchFMI split_key1 elt1 s1 left1 right1)
plusFM' (BranchFMI split_key1 elt1 s1 left1 right1)
           (BranchFMI split_key elt2 _ left right)
  = mkVBalBranch split_key elt2 (plusFM' lts left) (plusFM' gts right)
  where
    fm1 = BranchFMI split_key1 elt1 s1 left1 right1
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key

--- Efficiently combine key/element mappings of two maps into a single one,
--- cf. addToFM_C
plusFM_C :: (Int -> Int -> Int)
         -> FMI -> FMI -> FMI
plusFM_C combiner (FMI fm1) (FMI fm2) =
  FMI (plusFM_C' combiner fm1 fm2)

plusFM_C' :: (Int -> Int -> Int)
          -> FiniteMap -> FiniteMap -> FiniteMap
plusFM_C'  _        EmptyFMI fm2 = fm2
plusFM_C'  _        (BranchFMI split_key1 elt1 s1 left1 right1) EmptyFMI =
          BranchFMI split_key1 elt1 s1 left1 right1
plusFM_C' combiner (BranchFMI split_key1 elt1 s1 left1 right1)
                      (BranchFMI split_key elt2 _ left right)
  = mkVBalBranch split_key new_elt
                 (plusFM_C' combiner lts left)
                 (plusFM_C' combiner gts right)
  where
    fm1 = BranchFMI split_key1 elt1 s1 left1 right1
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key
    new_elt = case lookupFM' fm1 split_key of
                Nothing    -> elt2
                Just elt1' -> combiner elt1' elt2

--- (minusFM a1 a2) deletes from a1 any bindings which are bound in a2
minusFM :: FMI -> FMI -> FMI
minusFM (FMI fm1) (FMI fm2) = FMI (minusFM' fm1 fm2)

minusFM' :: FiniteMap -> FiniteMap -> FiniteMap
minusFM'  EmptyFMI _ = EmptyFMI
minusFM'  (BranchFMI split_key1 elt1 s1 left1 right1) EmptyFMI =
  BranchFMI split_key1 elt1 s1 left1 right1
minusFM' (BranchFMI split_key1 elt1 s1 left1 right1)
          (BranchFMI split_key _ _ left right)
  = glueVBal (minusFM' lts left) (minusFM' gts right)
       -- The two can be way different, so we need glueVBal
  where
    fm1 = BranchFMI split_key1 elt1 s1 left1 right1
    lts = splitLT fm1 split_key  -- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key  -- are not in either.

--- Filters only those keys that are bound in both of the given maps.
--- The elements will be taken from the second map.
intersectFM :: FMI-> FMI -> FMI
intersectFM (FMI fm1) (FMI fm2) = FMI (intersectFM' fm1 fm2)

intersectFM' :: FiniteMap -> FiniteMap -> FiniteMap
intersectFM' fm1 fm2 = intersectFM_C' (\ _ right -> right) fm1 fm2

--- Filters only those keys that are bound in both of the given maps
--- and combines the elements as in addToFM_C.
intersectFM_C :: (Int -> Int -> Int) -> FMI-> FMI -> FMI
intersectFM_C combiner (FMI fm1) (FMI fm2) =
  FMI (intersectFM_C' combiner fm1 fm2)

intersectFM_C' :: (Int -> Int -> Int)
               -> FiniteMap -> FiniteMap -> FiniteMap
intersectFM_C'  _        _        EmptyFMI = EmptyFMI
intersectFM_C'  _        EmptyFMI (BranchFMI _ _ _ _ _) = EmptyFMI
intersectFM_C' combiner (BranchFMI split_key1 elt1 s1 left1 right1)
                           (BranchFMI split_key elt2 _ left right)

  | isJust maybe_elt1   -- split_elt *is* in intersection
  = mkVBalBranch  split_key (combiner elt1' elt2)
                 (intersectFM_C' combiner lts left)
                 (intersectFM_C' combiner gts right)

  | otherwise           -- split_elt is *not* in intersection
  = glueVBal  (intersectFM_C' combiner lts left)
              (intersectFM_C' combiner gts right)

  where
    fm1 = BranchFMI split_key1 elt1 s1 left1 right1
    lts = splitLT fm1 split_key      -- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key      -- are not in either.

    maybe_elt1 = lookupFM' fm1 split_key
    Just elt1'  = maybe_elt1

-------------------------------------------------------------
--  MAPPING, FOLDING, FILTERING on finite maps
-------------------------------------------------------------

-- --- Folds finite map by given function.
foldFM :: (Int -> Int -> a -> a) -> a -> FMI -> a
foldFM k z (FMI fm) = foldFM' k z fm

foldFM' :: (Int -> Int -> a -> a) -> a -> FiniteMap -> a
foldFM'  _ z EmptyFMI = z
foldFM'  k z (BranchFMI key elt _ fm_l fm_r)
  = foldFM' k (k key elt (foldFM' k z fm_r)) fm_l

-- --- Applies a given function on every element in the map.
-- mapFM :: (key -> elt1 -> elt2) -> FM key elt1 -> FM key elt2
-- mapFM f (FM le fm) = FM le (mapFM' le f fm)
-- 
-- mapFM' :: LeKey key -> (key -> elt1 -> elt2)
--        -> FiniteMap key elt1 -> FiniteMap key elt2
-- mapFM' _  _ EmptyFMI = EmptyFMI
-- mapFM' le f (BranchFMI key elt size fm_l fm_r)
--   = BranchFMI key (f key elt) size (mapFM' le f fm_l) (mapFM' le f fm_r)
-- 
-- --- Yields a new finite map with only those key/element pairs matching the
-- --- given predicate.
-- filterFM  :: (key -> elt -> Bool) -> FM key elt -> FM key elt
-- filterFM p (FM le fm) = FM le (filterFM' le p fm)
-- 
-- filterFM' :: LeKey key -> (key -> elt -> Bool)
--           -> FiniteMap key elt -> FiniteMap key elt
-- filterFM' _  _ EmptyFMI = EmptyFMI
-- filterFM' le p (BranchFMI key elt _ fm_l fm_r)
--   | p key elt          -- Keep the item
--   = mkVBalBranch le key elt (filterFM' le p fm_l) (filterFM' le p fm_r)
-- 
--   | otherwise          -- Drop the item
--   = glueVBal le (filterFM' le p fm_l) (filterFM' le p fm_r)

-----------------------------------------------------
-- INTERROGATING finite maps
-----------------------------------------------------

--- How many elements does given map contain?
sizeFM :: FMI -> Int
sizeFM (FMI EmptyFMI)               = 0
sizeFM (FMI (BranchFMI _ _ size _ _)) = size

sizeFM' :: FiniteMap -> Int
sizeFM' EmptyFMI              = 0
sizeFM' (BranchFMI _ _ size _ _) = size

--- Do two given maps contain the same key/element pairs?
eqFM :: FMI -> FMI -> Bool
fm_1 `eqFM` fm_2 =
  (sizeFM   fm_1 == sizeFM   fm_2) &&   -- quick test
  (fmToList fm_1 == fmToList fm_2)

--- Is the given finite map empty?
isEmptyFM        :: FMI -> Bool
isEmptyFM fm = sizeFM fm == 0

--- Does given map contain given key?
elemFM :: Int -> FMI  -> Bool
key `elemFM` fm = isJust (lookupFM fm key)

--- Retrieves element bound to given key
lookupFM :: FMI -> Int -> Maybe Int
lookupFM (FMI fm) key = lookupFM' fm key

lookupFM' :: FiniteMap -> Int -> Maybe Int
lookupFM'  EmptyFMI _   = Nothing
lookupFM' (BranchFMI key elt _ fm_l fm_r) key_to_find
  = if key_to_find < key
    then lookupFM' fm_l key_to_find
    else if key_to_find==key
         then Just elt
         else lookupFM' fm_r key_to_find


--- Retrieves element bound to given key.
--- If the element is not contained in map, return
--- default value.
lookupWithDefaultFM :: FMI -> Int -> Int -> Int
lookupWithDefaultFM fm deflt key
  = case lookupFM fm key of
      Nothing -> deflt
      Just elt -> elt

--- Retrieves the ordering on which the given finite map is built.
keyOrder :: FMI -> (Int->Int->Bool)
keyOrder (FMI _) = (<)

--- Retrieves the smallest key/element pair in the finite map
--- according to the basic key ordering.
minFM :: FMI -> Maybe (Int,Int)
minFM = min . tree
  where
   min EmptyFMI            = Nothing
   min (BranchFMI k x _ l _) | l==EmptyFMI = Just (k,x)
                            | otherwise  = min l

--- Retrieves the greatest key/element pair in the finite map
--- according to the basic key ordering.
maxFM :: FMI -> Maybe (Int,Int)
maxFM = max . tree
  where
    max EmptyFMI            = Nothing
    max (BranchFMI k x _ _ r) | r==EmptyFMI = Just (k,x)
                             | otherwise  = max r



-- ----------------------------------------------------
-- -- LISTIFYING: transform finite maps to lists
-- ----------------------------------------------------

--- Builds a list of key/element pairs. The list is ordered
--- by the initially given irreflexive order predicate on keys.
fmToList        :: FMI -> [(Int,Int)]
fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm

--- Retrieves a list of keys contained in finite map.
--- The list is ordered
--- by the initially given irreflexive order predicate on keys.
keysFM                :: FMI -> [Int]
keysFM fm   = foldFM (\ key _   rest -> key : rest)       [] fm

--- Retrieves a list of elements contained in finite map.
--- The list is ordered
--- by the initially given irreflexive order predicate on keys.
eltsFM                :: FMI -> [Int]
eltsFM fm   = foldFM (\ _   elt rest -> elt : rest)       [] fm

--- Retrieves list of key/element pairs in preorder of the internal tree.
--- Useful for lists that will be retransformed into a tree or to match
--- any elements regardless of basic order.

fmToListPreOrder :: FMI -> [(Int,Int)]
fmToListPreOrder (FMI fm) = pre fm []
   where
     pre EmptyFMI xs = xs
     pre (BranchFMI k x _ l r) xs = (k,x):pre l (pre r xs)

--- Sorts a given list by inserting and retrieving from finite map.
--- Duplicates are deleted.
-- fmSortBy :: LeKey -> [Int] -> [Int]
-- fmSortBy p l = keysFM (listToFM p (zip l (repeat ())))
-- 
-----------------------------------------------------
-- reading/showing finite maps
-----------------------------------------------------

--- Transforms a finite map into a string. For efficiency reasons,
--- the tree structure is shown which is valid for reading only if one
--- uses the same ordering predicate.
showFM :: FMI -> String
showFM (FMI fm) = showQTerm fm

--- Transforms a string representation of a finite map into a finite map.
--- One has two provide the same ordering predicate as used in the
--- original finite map.
readFM :: String -> FMI
readFM s = FMI (readQTerm s)

-----------------------------------------------------
-- internal Implementation
-----------------------------------------------------

data FMI = FMI (FiniteMap)

tree :: FMI -> FiniteMap
tree (FMI fm) = fm

data FiniteMap
  = EmptyFMI
  | BranchFMI Int Int             -- Key and elt stored here
    Int{-STRICT-}              -- Size >= 1
    (FiniteMap)        -- Children
    (FiniteMap)

isEmptyFM' :: FiniteMap -> Bool
isEmptyFM' fm = sizeFM' fm == 0

-------------------------------------------------------------------------
--                                                                      -
--  The implementation of balancing                                     -
--                                                                      -
-------------------------------------------------------------------------
-------------------------------------------------------------------------
--                                                                      -
--  Basic construction of a FiniteMap                                   -
--                                                                      -
-------------------------------------------------------------------------
sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: Int
         -> Int -> Int
         -> FiniteMap -> FiniteMap
         -> FiniteMap

mkBranch _{-which-} key elt fm_l fm_r =
    let result = BranchFMI key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
      result
      --    if sizeFM result <= 8 then
      --     result
      --    else
      --      pprTrace ("mkBranch:"++(show which)) (ppr result) (
      --      result
      --      )
  where
    {-left_ok  = case fm_l of
                 EmptyFMI                         -> True
                 BranchFMI _ _ _ _ _  -> cmpWithBiggest_left_key key

    cmpWithBiggest_left_key key' = le (fst (findMax fm_l)) key'

    right_ok = case fm_r of
                 EmptyFMI                         -> True
                 BranchFMI _ _ _ _ _ -> cmpWithSmallest_right_key key

    cmpWithSmallest_right_key key' = le key' (fst (findMin fm_r))

    balance_ok = True -- sigh-}
    left_size  = sizeFM' fm_l
    right_size = sizeFM' fm_r


    unbox :: Int -> Int
    unbox x = x


-------------------------------------------------------------------------
--                                                                        -
-- Balanced construction of a FiniteMap                                 -
--                                                                        -
-------------------------------------------------------------------------
mkBalBranch :: Int -> Int
            -> FiniteMap -> FiniteMap
            -> FiniteMap

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l        -- Right tree too big
  = case fm_R of
        BranchFMI _ _ _ fm_rl fm_rr ->
              if sizeFM' fm_rl < 2 * sizeFM' fm_rr
                then single_L fm_L fm_R
                else double_L fm_L fm_R
        -- Other case impossible
        EmptyFMI -> error "FiniteMap.mkBalBranch"

  | size_l > sIZE_RATIO * size_r        -- Left tree too big
  = case fm_L of
        BranchFMI _ _ _ fm_ll fm_lr ->
              if sizeFM' fm_lr < 2 * sizeFM' fm_ll
                then single_R fm_L fm_R
                else double_R fm_L fm_R
        -- Other case impossible
        EmptyFMI -> error "FiniteMap.mkBalBranch"

  | otherwise                                -- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = sizeFM' fm_L
    size_r   = sizeFM' fm_R

    single_L fm_l (BranchFMI key_r elt_r _ fm_rl fm_rr)
        = mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr
    single_L _ EmptyFMI = error "FiniteMap.single_L"

    double_L fm_l (BranchFMI key_r elt_r _ (BranchFMI key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
        = mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
                                 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)
    double_L _ EmptyFMI = error "FiniteMap.double_L"
    double_L _ (BranchFMI _ _ _ EmptyFMI _) = error "FiniteMap.double_L"

    single_R (BranchFMI key_l elt_l _ fm_ll fm_lr) fm_r
        = mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)
    single_R EmptyFMI _ = error "FiniteMap.single_R"

    double_R (BranchFMI key_l elt_l _ fm_ll (BranchFMI key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
        = mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
                                 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)
    double_R EmptyFMI _ = error "FiniteMap.double_R"
    double_R (BranchFMI _ _ _ _ EmptyFMI) _ = error "FiniteMap.double_R"


mkVBalBranch ::  Int -> Int
             -> FiniteMap -> FiniteMap
             -> FiniteMap

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--           (a) all keys in l are < all keys in r
--           (b) all keys in l are < key
--           (c) all keys in r are > key

mkVBalBranch  key elt EmptyFMI fm_r = addToFM' fm_r key elt
mkVBalBranch  key elt (BranchFMI key_l elt_l s_l fm_ll fm_lr) EmptyFMI =
   addToFM'  (BranchFMI key_l elt_l s_l fm_ll fm_lr) key elt

mkVBalBranch  key elt (BranchFMI key_l elt_l s_l fm_ll fm_lr)
                      (BranchFMI key_r elt_r s_r fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    fm_l = BranchFMI key_l elt_l s_l fm_ll fm_lr
    fm_r = BranchFMI key_r elt_r s_r fm_rl fm_rr
    size_l = sizeFM' fm_l
    size_r = sizeFM' fm_r

-------------------------------------------------------------------------
--                                                                        -
-- Gluing two trees together                                            -
--                                                                        -
-------------------------------------------------------------------------
glueBal :: FiniteMap -> FiniteMap
        -> FiniteMap

glueBal fm1 fm2 =
  if isEmptyFM' fm1
    then fm2
    else if isEmptyFM' fm2
           then fm1
           else
        -- The case analysis here (absent in Adams' program) is really to deal
        -- with the case where fm2 is a singleton. Then deleting the minimum means
        -- we pass an empty tree to mkBalBranch, which breaks its invariant.
             let (mid_key1, mid_elt1) = findMax fm1
                 (mid_key2, mid_elt2) = findMin fm2
             in
             if sizeFM' fm2 > sizeFM' fm1
               then mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin fm2)
               else mkBalBranch mid_key1 mid_elt1 (deleteMax fm1) fm2

glueVBal :: FiniteMap -> FiniteMap
         -> FiniteMap

glueVBal fm_l fm_r =
  if isEmptyFM' fm_l
    then fm_r
    else if isEmptyFM' fm_r
           then fm_l
           else
             let BranchFMI key_l elt_l _ fm_ll fm_lr = fm_l
                 BranchFMI key_r elt_r _ fm_rl fm_rr = fm_r
                 --(mid_key_l,mid_elt_l) = findMax fm_l
                 --(mid_key_r,mid_elt_r) = findMin fm_r
                 size_l = sizeFM' fm_l
                 size_r = sizeFM' fm_r
             in
               if sIZE_RATIO * size_l < size_r
               then
                 mkBalBranch key_r elt_r (glueVBal fm_l fm_rl) fm_rr
                else if sIZE_RATIO * size_r < size_l
                    then
                      mkBalBranch key_l elt_l fm_ll (glueVBal fm_lr fm_r)

                      -- We now need the same two cases as in glueBal above.
                    else glueBal fm_l fm_r

-------------------------------------------------------------------------
--                                                                        -
-- Local utilities                                                      -
--                                                                        -
-------------------------------------------------------------------------

splitLT, splitGT :: FiniteMap -> Int
                    -> FiniteMap

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT  EmptyFMI _ = EmptyFMI
splitLT (BranchFMI key elt _ fm_l fm_r) split_key
  = if split_key < key
    then splitLT fm_l split_key
    else if split_key == key
         then fm_l
         else mkVBalBranch key elt fm_l (splitLT fm_r split_key)

splitGT  EmptyFMI _ = EmptyFMI
splitGT  (BranchFMI key elt _ fm_l fm_r) split_key
  = if split_key < key
    then mkVBalBranch key elt (splitGT fm_l split_key) fm_r
    else if split_key == key
         then fm_r
         else splitGT fm_r split_key

findMin :: FiniteMap -> (Int,Int)
findMin EmptyFMI = error "FiniteMap.findMin: empty map"
findMin (BranchFMI key elt _ EmptyFMI _) = (key,elt)
findMin (BranchFMI _   _   _ (BranchFMI key_l elt_l s_l fm_ll fm_lr)_) =
      findMin (BranchFMI key_l elt_l s_l fm_ll fm_lr)

deleteMin :: FiniteMap -> FiniteMap
deleteMin  EmptyFMI                           = error "FiniteMap.deleteMin: empty map"
deleteMin  (BranchFMI _   _   _ EmptyFMI fm_r) = fm_r
deleteMin  (BranchFMI key elt _ (BranchFMI key_l elt_l s_l fm_ll fm_lr) fm_r) =
  mkBalBranch key elt (deleteMin (BranchFMI key_l elt_l s_l fm_ll fm_lr))
                         fm_r

findMax :: FiniteMap -> (Int,Int)
findMax EmptyFMI = error "FiniteMap.findMax: empty map"
findMax (BranchFMI key elt _ _ EmptyFMI) = (key,elt)
findMax (BranchFMI _   _   _ _  (BranchFMI key_r elt_r s_r fm_rl fm_rr)) =
  findMax (BranchFMI key_r elt_r s_r fm_rl fm_rr)

deleteMax :: FiniteMap -> FiniteMap
deleteMax  EmptyFMI                           = error "FiniteMap.deleteMax: empty map"
deleteMax  (BranchFMI _   _   _ fm_l EmptyFMI) = fm_l
deleteMax (BranchFMI key elt _ fm_l (BranchFMI key_r elt_r s_r fm_rl fm_rr)) =
  mkBalBranch key elt fm_l
              (deleteMax (BranchFMI key_r elt_r s_r fm_rl fm_rr))



-------------------------------------------------------------------------
--                                                                      -
--   FiniteSets---a thin veneer                                         -
--                                                                      -
-------------------------------------------------------------------------
-- type FiniteSet key = FM key ()
-- emptySet         :: (LeKey key) -> FiniteSet key
-- mkSet            :: (LeKey key) -> [key] -> FiniteSet key
-- isEmptySet       :: FiniteSet _ -> Bool
-- elementOf        :: key -> FiniteSet key -> Bool
-- minusSet         :: FiniteSet key -> FiniteSet key -> FiniteSet key
-- setToList        :: FiniteSet key -> [key]
-- union            :: FiniteSet key -> FiniteSet key -> FiniteSet key

-- emptySet = emptyFM
-- mkSet le xs = listToFM le [ (x, ()) | x <- xs]
-- isEmptySet = isEmptyFM
-- elementOf = elemFM
-- minusSet  = minusFM
-- setToList = keysFM
-- union = plusFM


