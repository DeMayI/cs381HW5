module Homework5 where
import HW5Types



rankC :: Cmd -> CmdRank
rankC ADD = (2,1)
rankC (LDI i) = (0,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC SWAP = (2,2)
rankC (POP k) = (k,0) --Mine
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
    Just x -> sem p s

sem :: Prog -> Stack -> Result
sem [] s = A s       -- no program: succeed with current stack
sem (c:cs) s =
  case semCmd c s of
    A s' -> sem cs s'
    TypeError -> TypeError  -- stop execution on Nothing

semCmd :: Prog -> Stack -> Result
semCmd _ _ = TypeError
-- ADD two ints from stack
semCmd ADD ((I i):(I i'):s) = A ((I (i + i')):s)
semCmd ADD _ = TypeError
-- LDI load an int onto the stack
semCmd (LDI i) s = A ((I i):s)
-- LDB Load a bool onto the stack
semCmd (LDB b) s = A ((B b):s)
-- MULT two ints from stack
semCmd MULT ((I i):(I i'):s) = A ((I (i * i')):s)
semCmd MULT _ = TypeError
-- DUP duplicate the top value
semCmd DUP ((I i):s) = A ((I i):(I i):s)
semCmd DUP ((B b):s) = A ((B b):(B b):s)
semCmd DUP _ = TypeError
-- SWAP the top two values on the stack
semCmd SWAP ((I i):(I i'):s) = A ((I i'):(I i):s)
semCmd SWAP ((B b):(B b'):s) = A ((B b'):(B b):s)
semCmd SWAP ((I i):(B b):s) = A ((B b):(I i):s)
semCmd SWAP ((B b):(I i):s) = A ((I i):(B b):s)
semCmd SWAP _ = TypeError
-- LEQ if top value is less than next, push True, otherwise push False
semCmd LEQ ((I i):(I i'):s) = A ((B(i <= i')):s)
semCmd LEQ _ = TypeError
-- IFELSE If top of stack is True, execute first, if false execute second
semCmd (IFELSE p1 p2) ((B True):s) = semCmd p1 s
semCmd (IFELSE p1 p2) ((B False):s) = semCmd p2 s
semCmd (IFELSE _ _) _ = TypeError
-- POP k values from the stack
semCmd (POP k) s = A (drop k s)
-- Undefined commands
semCmd _ _ = TypeError
