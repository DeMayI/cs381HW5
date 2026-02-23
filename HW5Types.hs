-- Homework 5 (types)
module HW5Types where

type Prog = [Cmd]

data Cmd
    = LDI Int
    | ADD
    | MULT
    | DUP
    | INC
    | SWAP
    | POP Int
    | IFELSE Prog Prog
    | LDB Bool
    | LEQ
    deriving (Eq, Show)

data Val
    = I Int
    | B Bool
    deriving (Eq, Show)

type Stack = [Val]

data Result
    = A Stack
    | RankError
    | TypeError
    deriving (Eq, Show)

-- Semantic function type
type D = Stack -> Stack

-- Rank types
type Rank = Int
type CmdRank = (Int, Int)
