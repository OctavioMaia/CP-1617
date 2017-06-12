import Cp
import List

wc_w_final :: [Char] -> Int
wc_w_final = wrapper . worker

wrapper = p2

worker = cataList $ either a b
	   where a = split (true)   (const 0)
	         b = split (sep.p1) (cond ((uncurry(&&)).(not.sep><p1)) (succ.p2.p2) (p2.p2))

sep c =  c == ' ' 
	  || c == '\n' 
	  || c == '\t'