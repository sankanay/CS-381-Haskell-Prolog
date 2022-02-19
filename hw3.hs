{------------------------------------------------
Name: Yash Sankanagouda
ONID: sankanay@oregonstate.edu
Class: CS 381 HW 3 (Syntax)
Date: 2/1/2022
------------------------------------------------}

-- Homework 3 template

module Sentence where

-- Grammar for the animal sentence language:
--
--   <sentence> ->  <noun> <verb> [<noun>]  
--               	|  <sentence> `and` <sentence>
--				 	
--   <noun> 	-> <adj> <noun> | <noun> `and` <noun>
--					| `cats` | `dogs` | `ducks` | `bunnies`

--   <verb>		->  `chase` | `cuddle` | `hug` | `scare`
--   <adj>		->	`silly` | `small` | `old` | `happy`

data Sentence
   = NVN Noun Verb Noun
   | NV Noun Verb
   | And Sentence Sentence
   | End
  deriving (Eq,Show)

data Adj = Silly | Small | Old | Happy -- finish adjectives
  deriving (Eq,Show)

data Noun =  -- finish
     NP Adj Noun  -- Noun phrase
    | NAnd Noun Noun  -- Finish noun and noun
	  | Cats | Dogs | Ducks | Bunnies		  -- list of nouns
  deriving (Eq,Show)

data Verb = Chase | Cuddle | Hug | Scare -- finish

  deriving (Eq,Show)


-- | The sentence: cats cuddle ducks and dogs cuddle ducks
ex1 :: Sentence
ex1 = NVN Cats Hug Dogs

ex2 :: Sentence
ex2 = NVN (NP Silly Cats) Hug Dogs

ex3 :: Sentence
ex3 = NVN (NAnd Dogs Cats) Chase Ducks

ex4 :: Sentence
ex4 = NVN (NAnd (NP Silly Dogs) Cats) Chase Ducks


-- | Build a sentence from a noun verb noun.
-- | buildS2 Cats Hug Cats
-- | NVN Cats Hug Cats

buildS2 :: Noun -> Verb -> Noun -> Sentence
buildS2 a b c = NVN a b c
-- finish

-- | Build a sentence from a noun verb 
-- | buildS1 Cats Hug 
-- | NV Cats Hug 

buildS1 :: Noun -> Verb -> Sentence
buildS1 a b = NV a b
-- finish


-- | Build a noun phrase from an adjective and noun
-- | buildNP Silly Dogs
-- | NP Silly Dogs

buildNP :: Adj -> Noun -> Noun
buildNP a b = NP a b
-- finish

-- | Build a noun conjunction from two nouns
-- | buildNAnd Dogs Cats
-- | NAnd Dogs Cats

buildNAnd :: Noun -> Noun -> Noun
buildNAnd a b = NAnd a b
-- finish

-- | Build a sentence that is a conjunction of a list of other sentences.
-- | conjunction [ex1, ex2]
-- | And (NVN Cats Hug Dogs) (NVN (NP Silly Cats) Hug Dogs)
--  
conjunction :: [Sentence] -> Sentence
conjunction []     = End
conjunction (a:as)| (as/=[]) = And a (conjunction as)
                  | (as==[]) = a
-- finish

-- | Pretty print a sentence.
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (And l r)   = pretty l ++ " and " ++ pretty r
pretty (NV s v)     = prettyNoun s ++ " " ++ prettyVerb v
pretty (End) = "."

-- | Pretty print a noun.
prettyNoun :: Noun -> String
prettyNoun Cats  = "cats"
prettyNoun Dogs = "dogs"
prettyNoun Ducks = "ducks"
prettyNoun Bunnies = "bunnies"
-- finish


prettyNoun (NP a n) = prettyAdj a ++ " " ++ prettyNoun n
prettyNoun (NAnd m n) = prettyNoun m ++ " and " ++prettyNoun n

-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb Chase  = "chase"
prettyVerb Cuddle = "cuddle"
prettyVerb Hug = "hug"
prettyVerb Scare = "scare"

-- finish

-- | Pretty print an adjective.
prettyAdj :: Adj -> String
prettyAdj Silly  = "silly"
prettyAdj Small = "small"
prettyAdj Old = "old"
prettyAdj Happy = "happy"
-- finish


-- | Does the sentence contain only cuddling and hugs?
-- | isNice ex2
-- |   True
isNice :: Sentence -> Bool
isNice (NVN _ Chase _)    = False
isNice (NVN _ Scare _)    = False
isNice (NVN _ Cuddle _)   = True
isNice (NVN _ Hug _)      = True
isNice (NV _ Chase )      = False
isNice (NV _ Scare)       = False
isNice (NV _ Cuddle)      = True
isNice (NV _ Hug)         = True
isNice (And l r)  | ((isNice l && isNice r) == True) = True
                  | otherwise = False
-- finish

-- |Count the number of words in a sentence
-- | wordCount ex4
--    6
--Helper function for word count
countHelper :: Noun -> Int
countHelper Cats = 1
countHelper Dogs = 1
countHelper Bunnies = 1
countHelper Ducks = 1
countHelper (NP _ n) = countHelper n + 1
countHelper (NAnd x y) = countHelper x + countHelper y + 1
wordCount :: Sentence -> Int
wordCount (NVN x _ y) = countHelper x + countHelper y + 1
wordCount (NV a _) = countHelper a + 1
wordCount (And l r ) = wordCount l + wordCount r + 1
-- finish

-----------------------MAIN---------------------------

{-main = do
  let s1 = buildS2 Cats Hug Bunnies
  let s2 = buildS1 Cats Cuddle
  let s3 = buildNP Silly Ducks
  let s4 = buildNAnd Dogs Cats
  let s5 = conjunction [s2, s1]
  let s6 = buildS2 s3 Chase s4
  let s7 = buildS1 Dogs Scare
  let s8 = conjunction [s1, s2, s7]
  let s9 = buildNP Old s3
  let s10 = buildS2 s9 Cuddle Cats
  putStrLn " s1 "
  print (s1)
  print (pretty(s1))
  putStrLn" Is nice s1 "
  print (isNice s1)
  putStrLn" Word count s1 "
  print (wordCount s1)
  putStrLn " s2 "
  print (s2)
  print (pretty(s2))
  putStrLn" Is nice s2 "
  print (isNice s2)
  putStrLn" Word count s2 "
  print (wordCount s2)
  putStrLn " s5 "
  print (s5)
  print (pretty(s5))
  putStrLn" Is nice s5 "
  print (isNice s5)
  putStrLn" Word count s5 "
  print (wordCount s5)  
  putStrLn " s6 "
  print (s6)
  print (pretty(s6))
  putStrLn" Is nice s6 "
  print (isNice s6)
  putStrLn" Word count s6 "
  print (wordCount s6)
  putStrLn " s7 "
  print (s7)
  print (pretty(s7))
  putStrLn" Is nice s7 "
  print (isNice s7)
  putStrLn" Word count s7 "
  print (wordCount s7)
  putStrLn " s8 "
  print (s8)
  print (pretty(s8))
  putStrLn" Is nice s8 "
  print (isNice s8)
  putStrLn" Word count s8 "
  print (wordCount s8)
  putStrLn " s10 "
  print (s10)
  print (pretty(s10))
  putStrLn" Is nice s10 "
  print (isNice s10)
  putStrLn" Word count s10 "
  print (wordCount s10)-}