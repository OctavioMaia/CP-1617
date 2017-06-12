import Cp
import Data.List
import System.Random
import Probability
import LTree
import Data.Text hiding (length,map,take,drop)

type Equipa = String
fork = Cp.split

jogo :: (Equipa, Equipa) -> Dist Equipa
jogo (e1 , e2) = D [(e1 , 1-r1 / (r1 + r2 )), (e2 , 1-r2 / (r1 + r2 ))] where
	r1 = rank e1
	r2 = rank e2

rank = pap ranks
ranks = [("Arouca", 5),("Belenenses", 3),("Benfica", 1),("Braga", 2),("Chaves", 5),("Feirense", 5),("Guimaraes", 2),("Maritimo", 3),("Moreirense", 4),("Nacional", 3),("P.Ferreira", 3),("Porto", 1),("Rio Ave", 4),("Setubal", 4),("Sporting", 1),("Estoril", 5)]

equipas :: [Equipa]
equipas = ["Arouca", "Belenenses", "Benfica", "Braga", "Chaves", "Feirense","Guimaraes", "Maritimo", "Moreirense", "Nacional", "P.Ferreira","Porto", "Rio Ave", "Setubal", "Sporting", "Estoril"]

getR :: [a ] -> IO (a, [a ])
getR x = do {
	i <- getStdRandom (randomR (0, length x - 1));
	return (x !! i , retira i x )
	} where retira i x = take i x ++ drop (i + 1) x

presort :: (Ord a, Ord b) => (b -> a) -> [b ] -> [b ]
presort f = map p2 . sort . (map (fork f id ))

pap :: Eq a => [(a, t)] -> a -> t
pap m k = unJust (lookup k m) where unJust (Just a) = a