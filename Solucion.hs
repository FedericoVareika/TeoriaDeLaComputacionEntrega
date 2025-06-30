{-# OPTIONS_GHC -fno-warn-tabs #-}

module TrabajoFinal where 


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

--  1.1 --

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

verifyB :: (DomB, SolB) -> Bool
verifyB =  

-------------------------------------
