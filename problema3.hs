import Cp
import BTree
import Exp
import GHC.IO.Exception

data B_tree a = Nil | Block  { leftmost :: B_tree a, block :: [(a, B_tree a)] } deriving (Show,Eq)

-----------------1---------------

--in
inB_tree = either (const Nil) (uncurry Block)

--out
outB_tree Nil = i1 ()
outB_tree (Block a b) = i2 (a,b)

--recee
recB_tree f = baseB_tree id f

--base
baseB_tree g f = id -|- (f >< (map (g >< f)))

--cata
cataB_tree g = g . (recB_tree (cataB_tree g)) . outB_tree

--ana
anaB_tree g = inB_tree . (recB_tree (anaB_tree g)) . g

--hylo
hyloB_tree f g = cataB_tree f . anaB_tree g

--Functor
instance Functor B_tree
	where fmap f = cataB_tree ( inB_tree . baseB_tree f id)

-----------------2---------------

-- inord
inordB_tree = cataB_tree (either nil join)
	where join = conc.(id><(foldr(++) []).(map cons)) 


-----------------3---------------

--largest
largestBlock = cataB_tree largestBlockAux
largestBlockAux = either (const 0) largest
	where largest (x,xs) = max x (max (length xs) (maximum(p2(unzip xs))))

-----------------4---------------

--mirror
mirrorB_tree = anaB_tree ((id -|- mirrorB_treeAux) . outB_tree)
mirrorB_treeAux (x,xs) = (p2(last xs), reverse(mirrorAux(x,reverse(uncurry zip((reverse><id)(unzip xs))))))

mirrorAux (x,(a,b):xs) = (a,x):xs

-----------------5---------------

--lsplit
lsplitB_tree []  = Left ()
lsplitB_tree [h] = Right ([],[(h,[])])

--lsplitB_tree (x:y:t) = façam voces

--qSort
qSortB_tree :: Ord a => [a] -> [a]
qSortB_tree = inordB_tree . (anaB_tree lsplitB_tree) 

-----------------6---------------
dotB_tree :: Show a => B_tree a -> IO ExitCode
dotB_tree = dotpict . bmap nothing (Just . show) . cB_tree2Exp
cB_tree2Exp = cataB_tree (either nul rest)
	where nul = (const (Var "nil"))
	      rest = ((uncurry Term) . split ((map p1) . p2) (cons . (split (p1) ((map p2).p2))))