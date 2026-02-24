import HW5Types



rankC :: Cmd -> CmdRank
rankC ADD = (2,1)
rankC (LDI i) = (0,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC SWAP = (2,2)
rankC (POP k) = (k,0)
rankC (IFELSE p1 p2) = (1,0)
rankC (LDB b) = (0,1)
rankC LEQ = (2,1)

rankB :: CmdRank -> Rank -> Bool
rankB (p, i) r = ((r - p) >= 0)

nRank :: CmdRank -> Rank -> Rank
nRank (p, i) r = r - p + i

minRank ::  Maybe Rank -> Maybe Rank -> Maybe Rank
minRank Nothing Nothing = Nothing
minRank Nothing _ = Nothing
minRank _ Nothing = Nothing
minRank x y 
    | x < y = x
    | otherwise = y


rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP ((IFELSE p1 p2):cs) r 
    | rankB (rankC (IFELSE p1 p2)) r && minRank (rankP p1 (nRank (rankC (IFELSE p1 p2)) r)) (rankP p2 (nRank (rankC (IFELSE p1 p2)) r)) /= Nothing = rankP cs (minRank (rankP p1 (nRank (rankC (IFELSE p1 p2)) r)) (rankP p2 (nRank (rankC (IFELSE p1 p2)) r)))
    | otherwise = Nothing
rankP (c:cs) r 
    | rankB (rankC c) r = rankP cs (nRank (rankC c) r)
    | otherwise = Nothing
