module Teste where

import Data.Char
import Cp
import List
import Nat

sq_of = cataNat(split (either zero add) (either one ((2+).p2)))

inv x = cataNat(split (either one add) (either (const (1-x)) ((*(1-x)).p2)))

-- inv x  = p2.(for (split ((*(1-x)).p1) ((uncurry(+)).((*(1-x))><id))) (1,1)) 
-- prop_inv :: Double -> Int -> Bool
-- prop_inv x n = ( ((1/x)-(inv x (n+1))) < ((1/x)-(inv x n)) )