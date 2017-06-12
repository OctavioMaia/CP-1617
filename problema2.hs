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


--quickcheck
lookahead_sep [] = True
lookahead_sep (c : l) = sep c

wc_w :: [Char] -> Int
wc_w [] = 0
wc_w (c : l) = if not(sep c) && lookahead_sep(l)
			 then wc_w l + 1
			 else wc_w l

check s = toInteger(wc_w s) == toInteger(wc_w_final s)