--Name: Isaac DeMay, Zachary Goldstein
--Date: 2/24/2026


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
rankC INC = (1,1)
rankC _ = (0,0)

--Returns true if a cmd is runnable and wouldn't cause the rank to drop below zero
rankB :: CmdRank -> Rank -> Bool
rankB (p, i) r = ((r - p) >= 0)

--Returns the new rank of the stack after the commmand has been applied
nRank :: CmdRank -> Rank -> Rank
nRank (p, i) r = r - p + i

--Returns the smallest rank, or nothing if either argument is nothing
minRank ::  Maybe Rank -> Maybe Rank -> Maybe Rank
minRank Nothing Nothing = Nothing
minRank Nothing _ = Nothing
minRank _ Nothing = Nothing
minRank x y 
    | x < y = x
    | otherwise = y


rankP :: Prog -> Rank -> Maybe Rank

--If the program is empty return the current rank
rankP [] r = Just r

--Handles the ifelse statement ranks,
--First checks to see ifelse statement itself is rank Safe
--Then checks each child progrm to make sure they are rank safe, returning nothing if one isn't rank safe
--And returning the minimum rank if both are rank safe
rankP ((IFELSE p1 p2):cs) r
  | rankB (rankC (IFELSE p1 p2)) r
    , let mr = minRank (rankP p1 r1) (rankP p2 r1)
          r1 = nRank (rankC (IFELSE p1 p2)) r
    , mr /= Nothing
  = case mr of
      Just r' -> rankP cs r'
      Nothing -> Nothing         

  | otherwise = Nothing

--Default case, Checks to see if the cmd is rank safe, if so calls rankp on the next command with the new rank
--If the cmd is not rank safe, return nothing
rankP (c:cs) r 
    | rankB (rankC c) r = rankP cs (nRank (rankC c) r)
    | otherwise = Nothing

--Calculates the rank of a stack
stackToRank :: Stack -> Rank
stackToRank [] = 0
stackToRank (x:xs) = 1 + stackToRank xs


--Wrapper function for sem,
--Calls rankP to ensure that the prog is rank safe
--if so it returns sem, otherwise it returns a rank error
run :: Prog -> Stack -> Result
run p s
  = case rankP p (stackToRank s) of
    Nothing -> RankError
    Just x -> sem p s

--Iterates through the program until it reaches an empty list, at which point it will return an empty stack
--If any typeerrors are detected, return a TypeError instead
sem :: Prog -> Stack -> Result
sem [] s = A s       -- no program: succeed with current stack
sem (c:cs) s =
  case semCmd c s of
    A s' -> sem cs s'
    TypeError -> TypeError  -- stop execution on Nothing

semCmd :: Cmd -> Stack -> Result
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
semCmd (IFELSE p1 p2) ((B True):s) = sem p1 s
semCmd (IFELSE p1 p2) ((B False):s) = sem p2 s
semCmd (IFELSE _ _) _ = TypeError
-- POP k values from the stack
semCmd (POP k) s = A (drop k s)
semCmd INC ((I i):s) = A ((I(i + 1)):s)

-- Undefined commands
semCmd _ _ = TypeError
