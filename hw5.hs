{------------------------------------------------
Name: Jim Landers & Yash Sankanagouda
ONID: sankanay@oregonstate.edu
Class: CS 381 HW 5 (Types)
Date: 2/17/2022
------------------------------------------------}

import Data.Maybe
import Data.Either

type Prog = [Cmd] 

data Val 
    = I Int 
    | B Bool
    deriving Show

data Stack 
    = A [Val]
    | Arr [Int]
    | TypeError 
    | RankError
    deriving Show

data Cmd  
    = LDI Int 
    | LDB Bool
    | ADD 
    | MULT 
    | DUP 
    | LEQ
    | IFELSE Prog Prog
    | DEC
    | SWAP
    | POP Int
    deriving Show 

type Rank    = Int
type CmdRank = (Int, Int)


prog1 = [ADD]
prog2 = [ADD, ADD, ADD]
prog3 = [LDI 7, ADD]

stack1 = (A [I 2])
stack2 = (A [I 2, I 3, I 4, I 5])
stack3 = (A [B True, I 3, I 4, I 5])

-- MAIN
run :: Prog -> [Int] -> Stack
run [] s = (Arr s)
run p s = stackToInts (semStatTC p (A (intsToVals s)))


{- "semStatTC for evaluating the stack program that first calls the function rankP to check
whether the stack program is rank correct. If the program with a given stack is rank correct 
then semStatTC calls semCmd to execute the program otherwise SemStatTC produces a RankError.
Your run function can call semStatTC." -}
-- Checks if Prog is rank-correct, and if so carries out operations on stack.
-- If this returns nothing, it means a TypeError has occurred.
semStatTC :: Prog -> Stack -> Stack
semStatTC [] s = s
semStatTC (p:ps) s
    | rankP (p:ps) (rankS s) == Nothing = RankError
    | tc p s == True                    = (semStatTC ps (semCmd p s))
    | otherwise                         = TypeError


-- execute Cmd on stack after it is verified to work.
semCmd :: Cmd -> Stack -> Stack
semCmd (LDI a) (A v)                 = (A ((I a):v))
semCmd (LDB a) (A v)                 = (A ((B a):v))
semCmd (ADD) (A ((I v1):(I v2):vs))  = (A ((I (v1 + v2)):vs))
semCmd (MULT) (A ((I v1):(I v2):vs)) = (A ((I (v1 * v2)):vs))
semCmd (DUP) (A (v:vs))              = (A (v:v:vs))
semCmd (LEQ) (A ((I v1):(I v2):vs))  = (A (B (v1 <= v2):vs))
semCmd (IFELSE p1 p2) (A ((B v1):vs))
  | v1 == True  = semStatTC p1 (A vs)
  | v1 == False = semStatTC p2 (A vs)
semCmd (DEC) (A ((I v):vs))          = (A ((I (v-1)):vs))
semCmd (SWAP) (A (v1:v2:vs))         = (A (v2:v1:vs))
semCmd (POP n) s                     = popVals n s





-- Returns CmdRank. The first digit is how many Vals it pops off, and the second
-- is how many it puts back on.
rankC :: Cmd -> CmdRank
rankC (LDI x)  = (0, 1)
rankC (LDB x)  = (0, 1)
rankC (ADD)    = (2, 1)
rankC (MULT)   = (2, 1)
rankC (DUP)    = (0, 1)
-- LEQ HERE (maybe?)
-- IFELSE HERE (maybe?)
rankC (DEC)    = (1, 1)
rankC (SWAP)   = (2, 2)
rankC (POP x)  = (x, 0)


-- Ranks a stack by returning the length.
rankS :: Stack -> Rank
rankS (A [])  = 0
rankS (A [x]) = 1
rankS (A x)   = length x
rankS _       = 0


-- Returns the rank of a stack after a given Cmd operates on it. Nothing should
-- result in a RankError in the stack.
rank :: Cmd -> Rank -> Maybe Rank
rank (LDI a) x = Just (x+1)
rank (LDB a) x = Just (x+1)
rank (ADD  ) x = if x <= 1 then Nothing else Just (x-1)
rank (MULT ) x = if x <= 1 then Nothing else Just (x-1)
rank (DUP  ) x = if x == 0 then Nothing else Just (x+1)
rank (LEQ  ) x = if x <= 1 then Nothing else Just (x-1)
rank (IFELSE p1 p2) r
  | ((rankP p1 (r-1)) == Nothing) || ((rankP p2 (r-1)) == Nothing) = Nothing
  | (rankP p1 (r-1)) >= (rankP p2 (r-1)) = (rankP p2 (r-1))
  | otherwise = (rankP p1 (r-1))
rank (DEC  ) x = if x == 0 then Nothing else Just x
rank (SWAP ) x = if (x < 2) then Nothing else Just x
rank (POP a) x = if x < a then Nothing else Just (x-a)


-- Returns the rank of a stack after a Prog operates on it. Nothing should
-- result in a RankError in the stack.
rankP :: Prog -> Rank -> Maybe Rank
rankP [] r         = Just r
rankP [x] r       = (rank x r)
rankP (x:xs) r    = (rank x r) >>= rankP xs


-- Simple typechecking. Potential RankErrors are ignored because this function only
-- recieves a Cmd if that has been verified already.
-- (A good example of this is with POP.)
tc :: Cmd -> Stack -> Bool
tc (LDI a) (A v)                  = True
tc (LDI a) _                      = False
tc (LDB a) (A v)                  = True
tc (LDB a) _                      = False
tc (ADD) (A ((I v1):(I v2):vs))   = True
tc (ADD) _                        = False
tc (MULT) (A ((I v1):(I v2):vs))  = True
tc (MULT) _                       = False
tc (DUP) (A [])                   = False
tc (DUP) (A v)                    = True
tc (LEQ) (A ((I v1):(I v2):vs))   = True
tc (LEQ) (A (v:vs))               = False
tc (IFELSE p1 p2) (A ((B v1):vs)) = True
tc (IFELSE p1 p2) (A (v:vs))      = False
tc (DEC) (A ((I v1):vs))          = True
tc (DEC) _                        = False
tc (SWAP) (A (v1:v2:vs))          = True
tc (SWAP) _                       = False
tc (POP a) (A v)                  = True
tc (POP a) _                      = False




-- BASIC HELPER STUFF BELOW HERE

-- Simple helper-function to pop off n elements from a stack.
popVals :: Int -> Stack -> Stack
popVals n (A []) = (A [])
popVals 0 s = s
popVals n (A (v1:vs)) = popVals (n-1) (A vs)

intsToStack :: [Int] -> Stack
intsToStack x = A (intsToVals x)

intsToVals :: [Int] -> [Val]
intsToVals []     = []
intsToVals (x:xs) = (I x):(intsToVals xs)


stackToInts :: Stack -> Stack
stackToInts (Arr v)     = (Arr v)
stackToInts (RankError) = RankError
stackToInts (TypeError) = TypeError
stackToInts (A [])      = (Arr [])
stackToInts (A (v:vs))  = (Arr (valsToInts (v:vs)))


valsToInts :: [Val] -> [Int]
valsToInts []      = []
valsToInts (v:vs)  = (valToInt v):(valsToInts vs)


valToInt :: Val -> Int
valToInt (I n)     = n
valToInt (B True)  = 1
valToInt (B False) = 0