import Cp
import Nat
import Test.QuickCheck hiding ((><))

inv n = p1 . for (split (uncurry(+)) (((1-n)*).p2)) (1,1-n)

genDouble :: Gen(Double)
genDouble = choose (1.0,2.0)

test n = quickCheck $ forAll genDouble check
	   where check = \x -> (abs ((1/x)-(inv x n+1))) > abs ((1/x)-(inv x n))