{------------------------------------------------
Name: Yash Sankanagouda
ONID: sankanay@oregonstate.edu
Class: CS 381 HW 4 (Semantics) Part 2
Date: 2/8/2022
------------------------------------------------}
import Data.Maybe
import Data.Either

type Prog = [Cmd] 
type Stack = [Either Bool Int]

data Cmd  
    = LDI Int 
    | LDB Bool
    | ADD 
    | MULT 
    | DUP 
    | LEQ
    | IFELSE Prog Prog
    deriving Show 

-----------------------------------------------------------
--Testing Purposes--
{-stack1 :: Stack
stack2 :: Stack
stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
stack2 = [Left True, Right 3]
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]-}
--Testing Purposes--
-----------------------------------------------------------
run :: Prog -> Stack -> Maybe Stack
run [] x = Just x
run ((LDI x):xs) y = run xs (Right x:y)
run ((LDB x):xs) y = run xs (Left x:y)
run (ADD:xs) (Right a:Right b:bs) = run xs (Right(a+b):bs)
run (MULT:xs) (Right a:Right b:bs) = run xs (Right(a*b):bs)
run (DUP:xs) (Left a:as) = run xs (Left a:Left a: as)
run (DUP:xs) (Right a:as) = run xs (Right a:Right a: as)
run (LEQ:xs) (Right a:Right b:bs)   | (a<=b) = run xs (Left True:bs)
                                    | otherwise = run xs (Left False:bs) 
run ((IFELSE x y):ys) (Left a:as)   | (a==True) = run (x++ys) as 
                                    | otherwise = run (y++ys) as
run _ _ = Nothing
-----------------------------------------------------------
--NON WORKING PARTS--
--run ((LDI x):y) ys = run y (x:ys)
--run ((LDI x):xs) (Right a:cs) = run xs ((Right x):Right a:cs)
--run x [] = Nothing
--run x [] = Nothing

{-run (ADD:xs) [] = Nothing
run (ADD:xs) (Right a:bs) = Nothing
run (ADD:xs) (Left a:bs) = Nothing-}

{-run (MULT:xs) [] = Nothing
run (MULT:xs) (Right a:[]) = Nothing
run (MULT:xs) (Right a: Left b:bs) = Nothing-}
--run _ _ = Nothing

--run (DUP:xs) [] = Nothing
--run (DUP:xs) (Left a:[]) = run xs (Left a:Left a:[])

{-run (LEQ:xs) [] = Nothing
run (LEQ:xs) (Right a:[]) = Nothing
run (LEQ:xs) (Right a: Left b:[]) = Nothing-}

{-run ((IFELSE x y):ys) [] = Nothing
run ((IFELSE x y):ys) (Right a:[]) = Nothing
run ((IFELSE x y):ys) (Right a:as) = Nothing-}
--NON WORKING PARTS--
-----------------------------------------------------------