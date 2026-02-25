module Homework5 where
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
rankC _ = (0,0)

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
  | rankB (rankC (IFELSE p1 p2)) r
    , let mr = minRank (rankP p1 r1) (rankP p2 r1)
          r1 = nRank (rankC (IFELSE p1 p2)) r
    , mr /= Nothing
  = case mr of
      Just r' -> rankP cs r'
      Nothing -> Nothing         

  | otherwise = Nothing
rankP (c:cs) r 
    | rankB (rankC c) r = rankP cs (nRank (rankC c) r)
    | otherwise = Nothing

stackToRank :: Stack -> Rank
stackToRank [] = 0
stackToRank (x:xs) = 1 + stackToRank xs


run :: Prog -> Stack -> Result
run p s
  = case rankP p (stackToRank s) of
    Nothing -> RankError
    Just x -> semCmd p s

semCmd :: Prog -> Stack -> Result
semCmd _ _ = TypeError