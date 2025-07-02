{-# OPTIONS_GHC -fno-warn-tabs #-}
{- HLINT ignore "Use camelCase" -}

module Solucion where 

import Data.List
import GHC.RTS.Flags (RTSFlags(concurrentFlags))

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

{-
 - verifyA corre verifyPhi para todo elemento del dominio. 
   - verifyPhi corre verifyTerm para cada termino en si (3) 
     - verifyTerm busca la valuacion en la solucion dada para su variable 
     - Su orden es O(n)
   - Su orden es O(n)
 - Su orden es O(n^2) --> verifyA es polinomial
 -
 - -}

----------------- B -----------------

verifyCostB :: Cost -> SolB -> Bool
verifyCostB c ps = let costs = map (\(_, c, _) -> c) ps in
  sum costs <= c

verifyBenefitsB :: Benefit -> SolB -> Bool
verifyBenefitsB b ps = let benefits = map (\(_, _, b) -> b) ps in
  sum benefits >= b

groupIsSatisfied :: SolB -> G -> Bool
groupIsSatisfied ps g = 
    let projectNames = map (\(name, _, _) -> name) ps in 
    all (`elem` projectNames) g 

getSolutionGroups :: SolB -> [G] -> [G]
getSolutionGroups ps gs =
  let projectNames = map (\(name, _, _) -> name) ps in 
  filter (\g -> any (`elem` g) projectNames) gs

verifyB :: (DomB, SolB) -> Bool
verifyB ((ps, gs, cost, benefit), sol) = 
  verifyCostB cost sol &&
  verifyBenefitsB benefit sol &&
  all (groupIsSatisfied sol) (getSolutionGroups sol gs) 

{-
 - verifyB: corre
   - verifyCostB: suma todos los costos de su solucion (O(n))
   - verifyBenefitsB: suma todos los beneficios de su solucion (O(n))
   - getSolutionGroups: filtra todos los grupos por los que tienen proyectos en
     la solucion O(n^2)
   - groupIsSatisfied para todos los grupos filtrados: corre 
     - que todos los proyectos del grupo esten en la solucion (O(n^2))
   - O(n^3)
 - O(n^3) --> verifyB es polinomial
 -
 - -}

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

