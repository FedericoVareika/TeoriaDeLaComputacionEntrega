{-# OPTIONS_GHC -fno-warn-tabs #-}
{- HLINT ignore "Use camelCase" -}

module Solucion where 

import Data.List

--  1.1 --

----------------- A -----------------

-- Dominio 
type Var = String
type Term = (Var, Bool) 
type Phi = (Term, Term, Term)
type DomA = [Phi]

-- Solucion
type SolA = [(Var, Bool)]

----------------- B -----------------

-- Dominio
type Name = String
type Cost = Int
type Benefit = Int

type P = (Name, Cost, Benefit)
type G = [Name]

-- (..., Costo maximo, Beneficio minimo)
type DomB = ([P], [G], Cost, Benefit) 

-- Solucion
type SolB = [P]

-------------------------------------

--  1.2 --

----------------- A -----------------

verifyTerm :: Term -> SolA -> Bool
verifyTerm (var, modifyer) sol = 
  let maybeVal = lookup var sol in
  case maybeVal of 
    Just val -> val == modifyer

verifyPhi :: Phi -> SolA -> Bool 
verifyPhi (t1, t2, t3) s = 
  verifyTerm t1 s || verifyTerm t2 s || verifyTerm t3 s

verifyA :: (DomA, SolA) -> Bool
verifyA (dom, sol) = foldr (\phi -> (&&) $ verifyPhi phi sol) True dom

----------------- B -----------------

verifyCostB :: Cost -> SolB -> Bool
verifyCostB c ps = let costs = map (\(_, c, _) -> c) ps in
  sum costs <= c

verifyBenefitsB :: Benefit -> SolB -> Bool
verifyBenefitsB b ps = let benefits = map (\(_, _, b) -> b) ps in
  sum benefits >= b

getGroupIndex :: [G] -> Name -> Int -> Int
getGroupIndex [] name acc = -1
getGroupIndex (g:gs) name acc 
  | elem name g = acc
  | otherwise = getGroupIndex gs name (acc + 1)

verifyGroupB :: [G] -> SolB -> Bool
verifyGroupB gs ps = 
  let groups = map (\(name, _, _) -> getGroupIndex gs name 0) ps in
  let firstGroup = groups !! 1 in
  all (firstGroup ==) groups

groupIsSatisfied :: SolB -> G -> Bool
groupIsSatisfied ps g = 
    let projectNames = map (\(name, _, _) -> name) ps in 
    all (`elem` projectNames) g 

verifyB :: (DomB, SolB) -> Bool
verifyB ((ps, gs, cost, benefit), sol) = 
  verifyCostB cost sol &&
  verifyBenefitsB benefit sol &&
  all (groupIsSatisfied sol) gs 

-------------------------------------


--  1.3 --

----------------- A -----------------

addTermToSet :: Term -> [Var] -> [Var] 
addTermToSet (v, _) vs 
  | elem v vs = vs 
  | otherwise = v:vs


getVars :: DomA -> [Var]
getVars [] = [] 
getVars ((t1, t2, t3):ts) = 
  addTermToSet t1 $ addTermToSet t2 $ addTermToSet t3 $ getVars ts

getPossibleSolutionsA :: [Var] -> [SolA] 
getPossibleSolutionsA [] = [[]]
getPossibleSolutionsA (v:vs) = 
  let vsSolutions = getPossibleSolutionsA vs in 
  map ((v, False):) vsSolutions ++ map ((v, True):) vsSolutions
  
solveA :: DomA -> SolA
solveA dom =
  let possibleSolutions = getPossibleSolutionsA $ getVars dom in 
  let verifyableSolutions = map (dom,) possibleSolutions in
  case find verifyA verifyableSolutions of 
    Just (_, sol) -> sol 
    Nothing -> []

----------------- B -----------------

getPossibleSolutionsB :: [P] -> [SolB]
getPossibleSolutionsB [] = [[]] 
getPossibleSolutionsB (p:ps) = 
  let psSolutions = getPossibleSolutionsB ps in 
  map (p:) psSolutions ++ psSolutions 
    
solveB :: DomB -> SolB
solveB dom@(ps, gs, cost, benefit) = 
  let possibleSolutions = getPossibleSolutionsB ps in 
  let verifyableSolutions = map (dom,) possibleSolutions in
  case find verifyB verifyableSolutions of 
    Just (_, sol) -> sol 
    Nothing -> []

-------------------------------------

verify3SatTR :: DomA -> SolA -> Bool -> Bool
verify3SatTR [] sol acc = acc
verify3SatTR (c@(t1, t2, t3):cs) sol acc = 
  let terms = [t1, t2, t3] in
  let verifiedClause = foldr (\t -> (verifyTerm t sol ||)) False terms in
  verify3SatTR cs sol (acc && verifiedClause)

