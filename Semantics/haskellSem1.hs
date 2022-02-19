{------------------------------------------------
Name: Yash Sankanagouda
ONID: sankanay@oregonstate.edu
Class: CS 381 HW 4 (Semantics) Part 1
Date: 2/8/2022
------------------------------------------------}
import Data.Maybe

type Prog = [Cmd] 
data Cmd  
    = LD Int 
    | ADD 
    | MULT 
    | DUP 
    deriving Show 

type Stack = [Int] 
-----------------------------------------------------------
--Testing Purposes--
{-stack1 :: Stack
stack1  = [1,2,3,4,5]
test1   = [LD 3, DUP, ADD, DUP, MULT]
test2   = [LD 3, ADD]
test3   = []
test4   = [ADD, ADD, ADD, ADD]-}
--Testing Purposes--
-----------------------------------------------------------
addS :: Stack -> Stack --Add stack helper function
addS (s1:s2:ss) = (s1+s2):ss
multS :: Stack -> Stack --Multiply stack helper function
multS (s1:s2:ss) = (s1*s2):ss

run :: Prog -> Stack -> Maybe Stack 
run [] x        = Just x
run ((LD x):y) ys = run y (x:ys)
run x [] = Nothing
run (ADD:xs) x
        | (length x >= 2) =  run xs (addS x)
        | otherwise = Nothing
run (MULT:xs) x
        | (length x >= 2) =  run xs (multS x)
        | otherwise = Nothing
run (DUP:xs) (x:ys) = run xs (x:x:ys)
-----------------------------------------------------------
--NON WORKING PARTS--
--run (x:xs) y    = run xs (semCmd x (Just y))
--run [ADD] (x:y:ys) = Just ((x+y):ys)
--run (MULT:xs) (x:y:ys)

{-semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LD x) (xs)      =  Just (x:xs)
semCmd ADD (x:y:ys)     
            | (length (x:y:ys) >= 2) = Just ((x+y):ys)
--            |otherwise =  ((x+y):ys)
semCmd MULT (x:y:ys)    =  Just ((x*y):ys)
semCmd DUP (x:xs)       =  Just ([x,x] ++ xs)-}

--run (x:xs) y | (xs/=[] = run xs run x y)
--NON WORKING PARTS--
-----------------------------------------------------------
