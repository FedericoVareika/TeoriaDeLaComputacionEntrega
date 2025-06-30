{-# OPTIONS_GHC -fno-warn-tabs #-}

module TrabajoFinal where 

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

getGroupIndex :: [G] -> Name -> Int
getGroupIndex [] name = -1
getGroupIndex (g:gs) name 
  | elem name g = 0
  | otherwise = getGroupIndex gs name + 1

verifyGroupB :: [G] -> SolB -> Bool
verifyGroupB gs ps = 
  let groups = map (\(name, _, _) -> getGroupIndex gs name) ps in
  let firstGroup = groups !! 1 in
  all (firstGroup ==) groups

verifyB :: (DomB, SolB) -> Bool
verifyB ((ps, gs, cost, benefit), sol) = 
  verifyCostB cost sol &&
  verifyBenefitsB benefit sol &&
  verifyGroupB gs sol

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
getPossibleSolutionsA [] = []
getPossibleSolutionsA (v:vs) = 
  let psSolutions = getPossibleSolutionsA vs in 
  map ((v, False):) psSolutions ++ map ((v, True):) psSolutions
  
solveA :: DomA -> SolA
solveA dom =
  let possibleSolutions = getPossibleSolutionsA $ getVars dom in 
  let verifyableSolutions = map (dom,) possibleSolutions in
  case find verifyA verifyableSolutions of 
    Just (_, sol) -> sol 
    Nothing -> []

----------------- B -----------------

solveB :: DomB -> SolB
solveB = undefined

-------------------------------------
