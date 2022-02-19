{------------------------------------------------
Name: Yash Sankanagouda
ONID: sankanay@oregonstate.edu
Class: CS 381 HW 2 (Haskell)
Date: 1/24/2022
------------------------------------------------}

module HW2types where
import Data.List (nub,sort)
import Data.List
-- Types for Exercise 1
--

type Bag a = [(a,Int)]

--------------------------------------
--insert function
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x ((y,n):ys) 
            | x==y    = (y,n+1):ys
            | otherwise = (y,n):ins x ys

--------------------------------------
--delete function
{- not working part
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((y,n):ys) 
        | x == y, n > 1 = ((y,n-1):ys)
        | x == y, n == 1 = ys
        | otherwise = ((y,n) : del x ys)-}
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((y,n):ys) 
        | x==y, n>1    = (y,n-1):ys
        | x==y, n==1 = ys
        | otherwise = (y,n):del x ys

--------------------------------------
--bag list function
--listToBag = [7,3,8,7,3,2,7,5] //testing purpose
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

--------------------------------------
--subbag function
{- not working part
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = True
subbag x [] = False
subbag [] y = True
subbag (x:xs) (y:ys)
        | x==y = subbag xs ys
        | otherwise = subbag (x:xs) ys-}

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = True
subbag x [] = False
subbag [] y = True
subbag ((a,n):as) ((b,m):bs)
        | a/=b = subbag as bs
        | a==b = subbag as bs 

--------------------------------------
--is set function
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((x,n):xs)
        | n>1 = False
        | n==1 = isSet xs  

--------------------------------------
--size of bag
{- not working part
size :: Bag a -> Int
size [] = 0
size [(x,n)] = n
-}

test :: [(Int, Int)]
test = [(5,1),(7,3),(2,1),(3,2),(8,1)]
size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size xs

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Types and functions for Exercise 2
--

g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
norm :: Ord a => [a] -> [a]
norm = sort . nub

--------------------------------------
--nodes contained in a graph
nodes :: Graph -> [Node]
nodes [] = []
nodes xs = nub ([x | (x,y) <- xs] `union` [y | (x,y) <- xs])

--------------------------------------
--successors for a node
suc :: Node -> Graph -> [Node]
suc n [] = []
suc n ((x,y):ys)
        | x==n  =[y] ++ suc n ys
        | otherwise = suc n ys

--------------------------------------
--detach node
detach :: Node -> Graph -> Graph
detach n [] = []
detach n ((x,y):ys)
        | x==n  = detach x ys
        | y==n  = detach y ys
        | otherwise = (x,y):detach n ys

--------------------------------------
--cycle of given number
{- not working part
cyc :: Int -> Graph
cyc 0 = []
cyc 1 = []
cyc x = (cyc (x-1) ++ [(x-1, x)]) ++ [(x,1)]
-}

ncyc :: Int -> Graph
cyc :: Int -> Graph
ncyc 0 = []
ncyc 1 = []
ncyc x = ncyc (x-1) ++ [(x-1, x)] 
cyc x = ncyc x ++ [(x,1)]

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Types for Exercise 3
--
type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

--------------------------------------
--compute the width
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2] --test 1
w = [Pt (4,4), Circle (20,60) 12, Rect (3,3) 50 2] -- test 2
width :: Shape -> Length
width (Pt x) = 0
width (Circle x y) = y*2
width (Rect x y z) = y

--------------------------------------
--compute the bounding box
bbox :: Shape -> BBox
bbox (Pt x) = (x,x)
bbox (Circle x y) = ((fst x - y ,snd x -y), (fst x+y, snd x+y))
bbox (Rect x y z) = ((x),(fst x + y, snd x + z))

--------------------------------------
--compute minimum x coordinate of shape
minX :: Shape -> Number
minX (Pt x) = fst x
minX (Circle x y) = fst x - y
minX (Rect x y z) = fst x

--------------------------------------
--move shape to position/add points helper function
{- not working part
myPoint = Pt (3,8)
move :: Shape -> Point -> Shape
move (Pt x) p = (fst x + fst p, snd x + snd p) 
-}
p = Pt (3,8)
c = Circle (5,2) 12
r = Rect (9, 10) 2 10
addPt :: Point -> Point -> Point
addPt (a,b) (c,d) = (a+c, b+d)
move :: Shape -> Point -> Shape
move (Pt x) p = Pt (addPt x p)-- > move (shape (cordinate)) 'coordinate to add'
move (Circle x y) p = Circle (addPt x p) y
move (Rect x y z) p = Rect (addPt x p) y z

---------------------------------------------------------------------
---------------------------------------------------------------------
-- main function
{-
main:: IO ()
main = 
        do
        let b1 = [(3,2), (4,1),(7,3),(12,5)]
        let b2 = [(3,1), (7,2)]
        let b3 = [(3,1), (4,1),(7,1),(10,1) ]
        putStrLn " b1 : "
        print b1
        putStrLn " b2 : "
        print b2
        putStrLn " b3 : "
        print b3
        putStrLn "Exercise 1  :"
        putStrLn " a) test 1 - ins 5 []  "
        print ( ins 5 [])
        putStrLn " a) test 2 - ins 5 b1 : "
        print ( ins 5 b1)
        putStrLn " a) test 3 -  ins 7 b1 : "
        print ( ins 7 b1)
        -- exercise 1 (b) del 
        putStrLn " b) test 1 -  del 5 [] "
        print ( del 5 [])
        putStrLn " b) test 2 - del 5 b1  "
        print ( del 5 b1)
        putStrLn " b) test 3 -  del 7 b1 "
        print ( del 7 b1)
        putStrLn " b) test 4 - del 7 b3 "
        print ( del 7 b3)
        -- exercise 1 (c) bag
        putStrLn " c) test 1 -  bag [1,2]  "
        print ( bag [1,2])
        putStrLn " c) test 2 -  bag [1,2,1,3,5,2]  "
        print ( bag [1,2,1,3,5,2])
        -- exercise 1 (d) subbag
        putStrLn " d) test 1 -  subbag b1 []  "
        print ( subbag b1[])
        putStrLn " d) test 2 -  subbag b1 b2  "
        print ( subbag b1 b2)
        putStrLn " d) test 3 -  subbag b2 b1  "
        print ( subbag b2 b1)
        putStrLn " e) test 1 -  isSet b1 "
        print ( isSet b1)
        putStrLn " e) test 2 -  isSet b2 "
        print ( isSet b2)
        putStrLn " e) test 3 -  isSet b3 "
        print ( isSet b3)
        putStrLn " f) test 1 -  size b1 "
        print ( size b1)
        putStrLn " f) test 2 -  size b2 "
        print ( size b2)
        putStrLn " f) test 3 -  size b3 "
        print ( size b3)
        let g = [ (1,2), (1,3), (2,3), (2,4),(3,4)]
        let h = [ (1,2), (1,3), (2,1), (3,2), (4,4) ]
        let g2 = [ (1,5), (1,7), (2,5), (7,2) ]
        putStrLn " g : "
        print g
        putStrLn " h : "
        print h
        putStrLn " g2 : "
        print g2
        putStrLn " "
        putStrLn "Exercise 2  :"
        putStrLn " a) test 1 - nodes g"
        print ( nodes g)
        putStrLn " a) test 2 - nodes h"
        print ( nodes h)
        putStrLn " a) test 3 - nodes g2"
        print ( nodes g2)
        putStrLn " b) test 1 - suc 4 g"
        print ( suc 4 g)
        putStrLn " b) test 2 - suc 2 g"
        print ( suc 2 g)
        putStrLn " b) test 3 - suc 4 h"
        print ( suc 4 h)
        putStrLn " c) test 1 - detach 4 g"
        print ( detach 4 g)
        putStrLn " c) test 2 - detach 3 g"
        print ( detach 3 g)
        putStrLn " c) test 2 - detach 3 h"
        print ( detach 3 h)
        putStrLn " d) test 1 - cyc 4 "
        print ( cyc 4 )
        putStrLn " "
        putStrLn "Exercise 3   :"
        let p1 = Pt (4,3)
        let c1 = Circle (5,5) 7
        let r1 = Rect (3,3) 5 3
        putStrLn " p1 : "
        print p1
        putStrLn " c1 : "
        print c1
        putStrLn " r1 : "
        print r1
        putStrLn " a) test 1 - width Point "
        print ( width p1)
        putStrLn " a) test 2 - width Circle "
        print ( width c1)
        putStrLn " a) test 3 - width Rect "
        print ( width r1)
        putStrLn " b) test 1 - bbox Point "
        print ( bbox p1)
        putStrLn " b) test 2 - bbox Circle "
        print ( bbox c1)
        putStrLn " b) test 3 - bbox Rect "
        print ( bbox r1)
        putStrLn " c) test 1 - minX Point "
        print ( minX p1)
        putStrLn " c) test 2 - minX Circle "
        print ( minX c1)
        putStrLn " c) test 3 - minX Rect "
        print ( minX r1)
        putStrLn " d) test 1 - move Point (3,-2)"
        print ( move p1 (3,-2))
        putStrLn " d) test 2 - move Circle (2,4)"
        print ( move c1 (2,4))
        putStrLn " d) test 3 - move Rect (-1,3)"
        print ( move r1 (-1,3))-}
