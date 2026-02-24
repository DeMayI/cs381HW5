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