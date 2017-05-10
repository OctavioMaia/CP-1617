inv :: Double -> Double -> Double
inv x 0 = 1
inv x 1 = (1-x)
inv x n = (conta (1-x) n) + inv x (n-1)

conta :: Double -> Double -> Double
conta x 0 = 1
conta x n = x * (conta x (n-1)) 
