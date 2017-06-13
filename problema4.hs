import Cp
import Probability hiding (choose)
import Test.QuickCheck hiding ((><))
import LTree

type Algae = A
data A = NA | A A B deriving Show
data B = NB | B A deriving Show

--int
inA = either (const NA)(uncurry A)
inB = either (const NB) B

--outs
outA NA = i1 ()
outA (A a b) = i2 (a,b)

outB NB = i1 ()
outB (B a) = i2 a

--catamorfismos
cataA ga gb = ga . (id -|- cataA ga gb >< cataB ga gb) . outA
cataB ga gb = gb . (id -|- cataA ga gb) . outB

--anamorfismos
anaA ga gb = inA . (id -|- anaA ga gb >< anaB ga gb) . ga
anaB ga gb = inB . (id -|- anaA ga gb) . gb

--generate
generateAlgae = anaA ga gb
ga 0 = i1 ()
ga n = i2 (n-1,n-1)
gb 0 = i1 ()
gb n = i2 (n-1)

showAlgae = cataA l r
			where l = either(const"A")(conc)
			      r = either(const"B")(id) 

--quickcheck
genInt :: Gen(Int)
genInt = choose (0,10)

check n = a == f
		where a = toInteger $ length(showAlgae(generateAlgae n))
		      f = fib $ toInteger(succ n)

test = quickCheck $ forAll genInt check