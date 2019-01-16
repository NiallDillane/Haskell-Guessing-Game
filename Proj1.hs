-- Author : Niall Dillane
-- Purpose: guess pairs of suspects to identify two criminals  

-- We repeatly input a pair of suspects with the character height,haircolor,sex
-- into system and system will give you the feedback of how many criminals
-- you guess write and automatically help you to generate the nextguess by using
-- the algorithm set inside the system and finally it will stop until we find the 
-- two right criminals and tell you how many guesses it takes. 


module Proj1 (Person, GameState, parsePerson, height, hair, sex, initialGuess,
              nextGuess, feedback, getMatches, getAtts, selectPairs, removePerson, 
              removePairs, select, removeSingle, removeKnown, removeSame) where

import System.IO
import Data.List

-- info to decide next guess based on previous guess
data GameState = GS SuspectPairs Prev Known
-- all the possible pairs of suspects
type SuspectPairs = [[Person]]
-- the pair of suspects guessed last time 
type Prev = [Person]
-- the known culprit of the pair, if/when we find one
type Known = String

--all possible suspects
data Person = SBM | SBF | SRM | SRF | SDM | SDF 
            | TBM | TBF | TRM | TRF | TDM | TDF
            deriving(Eq, Read, Show)

-- getters for the GameState
getSuspects (GS s _ _) = s
getPrev     (GS _ p _) = p 
getKnown    (GS _ _ k) = k


--takes 3-char string and returns person specified by string, or Nothing
parsePerson :: String -> Maybe Person
parsePerson p = Just (read p)
parsePerson _ = Nothing

--returns person's height, type in the Eq class
height :: Person -> Char
height p = (show p)!!0

--returns hair colour, type in the Eq class
hair :: Person -> Char
hair p = (show p)!!1

--returns sex, type in the Eq class
sex :: Person -> Char
sex p = (show p)!!2

--takes list of true culprits, list of suspects, returns quadruple of correct:
--suspects, heights, hair colours, sexes
feedback :: [Person] -> [Person] -> (Int,Int,Int,Int)
feedback [c1,c2] [s1,s2] = get [c1,c2] [s1,s2] (0,0,0,0)
  where
    get c s (cu,he,ha,se)
      | c1 == s1 && c2 == s2 = (cu+2,he,ha,se)
      | c1 == s2 && c2 == s1 = (cu+2,he,ha,se)
      | c1 == s1 = getMatches c2 s2 (1,0,0,0)
      | c1 == s2 = getMatches c2 s1 (1,0,0,0)
      | c2 == s1 = getMatches c1 s2 (1,0,0,0)
      | c2 == s2 = getMatches c1 s1 (1,0,0,0)
      | otherwise = (0,(getAtts height c s),(getAtts hair c s),(getAtts sex c s))

-- getters for matching persons and attributes
getMatches :: Person -> Person -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
getMatches c s (cu,he,ha,se) = get c s 0 (cu,he,ha,se)
  where
    get c s i (cu,he,ha,se)
      | i==0 && (height c == height s) = get c s 1 (cu,he+1,ha,se)
      | i==0 = get c s 1 (cu,he,ha,se)
      | i==1 && (hair c == hair s) = get c s 2 (cu,he,ha+1,se)
      | i==1 = get c s 2 (cu,he,ha,se)
      | i==2 && (sex c == sex s) = get c s 3 (cu,he,ha,se+1)
      | otherwise = (cu,he,ha,se)

getAtts :: (Person -> Char) -> [Person] -> [Person] -> Int
getAtts f [c1,c2] [s1,s2]
  | (f c1 == f s1 && f c2 == f s2) || (f c1 == f s2 && f c2 == f s1) = 2
  | f c1 == f s1 || f c2 == f s2 || f c1 == f s2 || f c2 == f s1 = 1
  | otherwise = 0


-- returns initial lineup, game state
initialGuess :: ([Person],GameState)
initialGuess = ([SBM,SDM], (GS [[x,y] | (x:ys) <- tails [SBM,TBM,SBF,TBF,SRM,TRM,SRF,TRF,SDM,TDM,SDF,TDF], y <- ys] [] ""))
                                                        
-- takes pair of previous guesses and game state (initialGuess & nextGuess)
-- and the feedback to this guess as a quadruple
-- returns pair of the next guess and new game state
nextGuess :: ([Person],GameState) -> (Int,Int,Int,Int) -> ([Person],GameState)
nextGuess ([g1,g2], GS s p k) (cu,he,ha,se)
  -- 
  -- if we get one right, set the flag to 1 and check which one it was, then populate 'known' so we don't loop here
  -- 
  | k == [] && cu == 1 = 
    ([g1,g1], GS s [g1,g2] "k")
    --
  | k == "k" && cu == 1 = 
    ( ( head (removeSecond g1 (gs))),
      GS (tail (removeSecond g1 (gs))) [g1,g1] (show g1))
    --
  | k == "k" && cu == 0 = 
    ( ( head (removeFirst g1 (gs))),
      GS (tail (removeFirst g1 (gs))) [g2,g2] (show prev2nd))
  -- 
  -- based on the initial guess (hence no previous guess) find combinations of height/hair/sex
  -- 
  | p == [] && cu == 0 && he == 0 && se == 0 = 
    ((head (removeHeSe g1 (gs))), 
      GS (tail (removeHeSe g1 (gs))) [g1,g2] k)
    --
  | p == [] && cu == 0 && he == 0 && se == 1 = 
    ((head (removeHeSe2 g1 (gs))), 
      GS (tail (removeHeSe2 g1 (gs))) [g1,g2] k)
    --
  | p == [] && cu == 0 && he == 1 && se == 1 = 
    ((head (removeHe2Se2 g1 (gs))), 
      GS (tail (removeHe2Se2 g1 (gs))) [g1,g2] k)
    -- 
  | p == [] && cu == 0 && he == 0 = 
    ((head (removeSingle height g1 (getSuspects (gs)))), 
      GS (tail (removeSingle height g1 (getSuspects(gs)))) [g1,g2] k)
    --
  | p == [] && cu == 0 && ha == 0 = 
    ((head (removeBothHairs g1 g2 (gs))), 
      GS (tail (removeBothHairs g1 g2 (gs))) [g1,g2] k)
    -- e.g. below, we see that our initial, same sexes returned 0,
    -- therefore we eliminate all suspects of that sex,
    -- we also preserve this in the gamestate
  | p == [] && cu == 0 && se == 0 = 
    ((head (removeSingle sex g1 (getSuspects (gs)))), 
      GS (tail (removeSingle sex g1 (getSuspects(gs)))) [g1,g2] k)
    --
  | p == [] && cu == 0 && se == 2 = 
    ((head (select sex g1 (getSuspects (gs)))), 
      GS (tail (select sex g1 (getSuspects(gs)))) [g1,g2] k)
  -- 
  -- checks when we have one guess right, but a wrong height/hair/sex
  -- 
  | cu == 1 && se == 0 && he == 0 && ha == 0 = 
    ((head (removeKAll [g1,g2] (gs))), 
      GS (tail (removeKAll [g1,g2] (gs))) [g1,g2] k)
    --
  | cu == 1 && se == 0 && he == 0 = 
    ((head (removeK2 height sex [g1,g2] (gs))), 
      GS (tail (removeK2 height sex [g1,g2] (gs))) [g1,g2] k)
    --
  | cu == 1 && se == 0 && ha == 0 = 
    ((head (removeK2 hair sex [g1,g2] (gs))), 
      GS (tail (removeK2 hair sex [g1,g2] (gs))) [g1,g2] k)
    -- e.g. below, height and hair were not matches,
    -- so we eliminate suspects with those attributes,
    -- checking that we don't remove the known culprit!
  | cu == 1 && he == 0 && ha == 0 = 
    ((head (removeK2 height hair [g1,g2] (gs))), 
      GS (tail (removeK2 height hair [g1,g2] (gs))) [g1,g2] k)
    --
  | cu == 1 && se == 0 = 
    ((head (removeK sex [g1,g2] (gs))), 
      GS (tail (removeK sex [g1,g2] (gs))) [g1,g2] k)
    --
  | cu == 1 && he == 0 = 
    ((head (removeK height [g1,g2] (gs))), 
      GS (tail (removeK height [g1,g2] (gs))) [g1,g2] k)
    --
  | cu == 1 && ha == 0 = 
    ((head (removeK hair [g1,g2] (gs))), 
      GS (tail (removeK hair [g1,g2] (gs))) [g1,g2] k)
  -- 
  -- one right but no checks succeeded, just pick randomly
  -- 
  | cu == 1 = 
    ( (head s), GS (tail s) [g1,g2] k)
  -- 
  -- neither right, but we got a 0 for hair matches
  -- 
  | cu == 0 && ha == 0 = 
    ((head (removeBothHairs g1 g2 (gs))), 
      GS (tail (removeBothHairs g1 g2 (gs))) [g1,g2] k)
  -- 
  -- neither were right, remove pairs with either of them
  -- 
  | otherwise = 
    (head (removePairs [g1,g2] (getSuspects (gs))), 
      GS (tail (removePairs [g1,g2] (getSuspects (gs)))) [g1,g2] k)
    -- functions used to break down the list of suspects
    where gs = (GS s p k)
          prev2nd = ((getPrev (gs))!!1)
          nc k [g1,g2] -- the suspect of a pair that wasn't the culprit
            | k == show g1 = g2
            | otherwise = g1
          -- 
          -- when we find our first culprit
          -- 
          removeFirst g1 (gs) = 
            (selectPairs prev2nd (removePerson g1 (getSuspects (gs))))
          removeSecond g1 (gs) = 
            (selectPairs g1 (removePerson prev2nd (getSuspects (gs))))
          -- 
          -- removing suspects based on the initial guess feedback
          -- 
          removeHeSe g1 (gs) = 
            (removeSingle height g1 (removeSingle sex g1 (getSuspects (gs))))
          removeHeSe2 g1 (gs) = 
            (removeSingle height g1 (removeSame sex (getSuspects (gs))))
          removeHe2Se2 g1 (gs) = 
            (removeSame height (removeSame sex (getSuspects (gs))))
          removeBothHairs g1 g2 (gs) = 
            (removeSingle hair g1 (removeSingle hair g2 (getSuspects (gs))))
          -- 
          -- removing suspects when we already have a known culprit
          -- 
          removeKAll g@[g1,g2] (gs) = 
            (removeKnown sex (nc k g) k (removeK2 height hair g (gs)))
          removeK2 f1 f2 g@[g1,g2] (gs) = 
            (removeKnown f1 (nc k g) k (removeKnown f2 (nc k g) k (getSuspects (gs))))
          removeK f g@[g1,g2] (gs) = 
            (removeKnown f (nc k g) k (getSuspects (gs)))
          
-- select all the pairs which include the now-known culprit
selectPairs :: Person -> [[Person]] -> [[Person]]
selectPairs s [] = []
selectPairs s (p:ps)  
    | s == (head p) || s == p!!1 = p : (selectPairs s ps)
    | otherwise = selectPairs s ps

-- remove pairs we know are incorrect
removePairs :: [Person] -> [[Person]] -> [[Person]]
removePairs [rm1,rm2] [] = []
removePairs [rm1,rm2] (p:ps)
  | rm1 == (head p) || rm1 == p!!1 = removePairs [rm1,rm2] ps
  | rm2 == (head p) || rm2 == p!!1 = removePairs [rm1,rm2] ps
  | otherwise = p : (removePairs [rm1,rm2] ps)

-- remove pairs that include a suspect we know is incorrect
removePerson :: Person -> [[Person]] -> [[Person]]
removePerson s [] = []
removePerson s (p:ps)
  | s == (head p) || s == p!!1 = removePerson s ps
  | otherwise = p : (removePerson s ps)


-- select only pairs which have a correct height/hair/sex in them
select :: (Person -> Char) -> Person -> [[Person]] -> [[Person]]
select f s [] = []
select f s (p:ps)  
    | (f s) == (f (head p)) && (f s) == (f (p!!1)) = p : (select f s ps)
    | otherwise = select f s ps

-- remove pairs where a suspect has the incorrect height/hair/sex
removeSingle :: (Person -> Char) -> Person -> [[Person]] -> [[Person]]
removeSingle f s [] = []
removeSingle f s (p:ps)  
    | ((f s) == (f (head p))) || ((f s) == (f (p!!1))) = (removeSingle f s ps)
    | otherwise = p : (removeSingle f s ps)

-- remove pairs where the height/hair/sex is wrong but make sure we check it against the known culprit
removeKnown :: (Person -> Char) -> Person -> String -> [[Person]] -> [[Person]]
removeKnown f s k [] = []
removeKnown f s k (p:ps)  
    | ((f s) == (f (head p))) && k /= (show (head p)) = (removeKnown f s k ps)
    | ((f s) == (f (p!!1))) && k /= (show (p!!1)) = (removeKnown f s k ps)
    | otherwise = p : (removeKnown f s k ps)

-- remove same-height/hair/sex pairs, since we know there's a mix
removeSame :: (Person -> Char) -> [[Person]] -> [[Person]]
removeSame f [] = []
removeSame f (p:ps)  
    | (f (head p)) == (f (p!!1)) = (removeSame f ps)
    | otherwise = p : (removeSame f ps)