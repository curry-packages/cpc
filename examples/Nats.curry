-- Some examples for the use of CurryCheck with user-defined data

import Test.Prop

-- Natural numbers defined by s-terms (Z=zero, S=successor):
data Nat = Z | S Nat

-- addition on natural numbers:
add         :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)

-- Property: the addition operator is commutative
add_commutative x y = add x y -=- add y x

