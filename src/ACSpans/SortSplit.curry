{- |
    Module      :  AST.SortSplit
    Description :  Functions for sorting and splitting declarations

    Declarations in the AST are in a different order than they are in the
    source code. There are two cases where this is important:
    1) Equations of a single function may be scattered all over the source code
    2) There are data declarations anywhere in the source code
    Because we have to stay synchronous with the Tokenstream we have to rewrite
    the AST by splitting FunctionDecls and sort all Decls by their position.
-}
module ACSpans.SortSplit (sortSplitModule, lePos) where

import Sort (quickSortBy)

import ACSpans.AST
import ACSpans.Span (Pos)
import ACSpans.PositionUtils (line)

-- |Reorganize the AST by splitting and sorting the declarations
sortSplitModule :: Module -> Module
sortSplitModule (Module mps mi mesp ids ds) =
  (Module mps mi mesp ids (quickSortBy lePos (concatMap splitFunctionDecl ds)))

-- |Traverse AST
sortSplitEq :: Equation -> Equation
sortSplitEq (Equation p lhs rhs) = (Equation p lhs (sortSplitRhs rhs))

-- |Sort and split local declarations
sortSplitRhs :: Rhs -> Rhs
sortSplitRhs rhs = case rhs of
    SimpleRhs  p   e  ds -> SimpleRhs  p e (qsDs ds)
    GuardedRhs ces ds    -> GuardedRhs ces (qsDs ds)
  where qsDs ds = quickSortBy lePos $ concatMap splitFunctionDecl ds

-- |Split FunctionDecls with more than one Equation
-- |into many FunctionDecls with only one Equation
-- |and call same function for local declarations recursively (sortSplitEq)
splitFunctionDecl :: Decl -> [Decl]
splitFunctionDecl d = case d of
  (FunctionDecl p i eqs) -> case eqs of
      []          -> []
      (eq : eqss) -> [(FunctionDecl (posEquation eq) i [sortSplitEq eq])]
                     ++ (splitFunctionDecl (FunctionDecl p i eqss))
  _                      -> [d]

-- |Get the position of an equation
-- |This belongs here and not into the PositionUtils-Module,
-- |because it refers to the simple AST, not the extended PosAST.
posEquation :: Equation -> Pos
posEquation (Equation p _ _) = p

-- ----------------------------------------------------------------------------
-- Utils for quicksort-algorithm on Declarations
-- ----------------------------------------------------------------------------

-- |`Less or equal` on Decls
lePos :: Decl -> Decl -> Bool
lePos d1 d2 = (line $ declPos d1) <= (line $ declPos d2)

-- |Get the position of a declaration.
-- |This belongs here and not into the PositionUtils-Module,
-- |because it refers to the simple AST, not the extended PosAST.
declPos :: Decl -> Pos
declPos (InfixDecl    p _ _ _  ) = p
declPos (DataDecl     p _ _ _  ) = p
declPos (NewtypeDecl  p _ _ _  ) = p
declPos (TypeDecl     p _ _ _  ) = p
declPos (TypeSig      p _ _    ) = p
declPos (FunctionDecl p _ _    ) = p
declPos (ForeignDecl  p _ _ _ _) = p
declPos (ExternalDecl p _      ) = p
declPos (PatternDecl  p _ _    ) = p
declPos (FreeDecl     p _      ) = p
