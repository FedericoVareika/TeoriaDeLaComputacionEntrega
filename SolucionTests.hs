module SolucionTests where 

import Solucion
import Data.List

-------- Para correr los tests de a/b --------
--                                          --
--   > verifyTests tests_to_verify_(a/b)    --
--                                          --
----------------------------------------------

-- Ejemplos para Dominio A

domA1 :: DomA
domA1 = [ (("x",True), ("y",False), ("z",True)), (("x",False), ("y",False), ("z",True)) ]

solA1, solA2 :: SolA
solA1 = [("x",True), ("y",False), ("z",True)]  -- asignación válida
solA2 = [("x",False), ("y",True), ("z",False)] -- asignación inválida para X

-- Tests para verifyTerm
-- verifyTerm debería devolver True
test_verifyTerm_true = verifyTerm ("x",True) solA1  -- True
-- verifyTerm debería devolver False
test_verifyTerm_false = verifyTerm ("x",True) solA2 -- False

-- Tests para verifyPhi
-- verifyPhi debería devolver True (al menos un término es True)
test_verifyPhi_true = verifyPhi (("x",True),("y",True),("z",False)) solA1  -- True
-- verifyPhi debería devolver False (ningún término es True)
test_verifyPhi_false = verifyPhi (("x",False),("y",True),("z",False)) solA1 -- False

-- Tests para verifyA
-- verifyA debería devolver True para domA1 y solA1
test_verifyA_true = verifyA (domA1, solA1)  -- True
-- verifyA debería devolver False para domA1 y solA2
test_verifyA_false = verifyA (domA1, solA2) -- False

-- Ejemplos para Dominio B

ps :: [P]
ps = [("p1",2,5), ("p2",3,4)]
gs :: [G]
gs = [["p1","p2"]]
domB, domB_lowCost :: DomB
domB = (ps, gs, 5, 9)             -- costo max 5, ben mín 9
domB_lowCost = (ps, gs, 4, 9)   -- costo max 4 (insuficiente)

solB_ok :: SolB
solB_ok = [("p1",2,5), ("p2",3,4)]  -- cumple todas las restricciones

-- Tests para verifyCostB
-- debería devolver True (2+3 <= 5)
test_verifyCostB_true = verifyCostB 5 solB_ok  -- True
-- debería devolver False (2+3 > 4)
test_verifyCostB_false = verifyCostB 4 solB_ok -- False

-- Tests para verifyBenefitsB
-- debería devolver True (5+4 >= 9)
test_verifyBenefitsB_true = verifyBenefitsB 9 solB_ok  -- True
-- debería devolver False (5+4 < 10)
test_verifyBenefitsB_false = verifyBenefitsB 10 solB_ok -- False

-- Tests para getGroupIndex
-- "p1" y "p2" están en el grupo 0
test_getGroupIndex_found = getGroupIndex gs "p2" 0  -- 0
-- nombre no existe, devuelve -1
test_getGroupIndex_notfound = getGroupIndex gs "x" 0 -- -1

-- Tests para groupIsSatisfied
test_groupIsSatisfied_true = groupIsSatisfied solB_ok ["p1","p2"]  -- True
test_groupIsSatisfied_false = groupIsSatisfied [("p1",2,5)] ["p1","p2"] -- False

-- Tests para verifyB
-- debería devolver True (cumple costo, beneficio y grupos)
test_verifyB_true = verifyB (domB, solB_ok)          -- True
-- debería devolver False (no cumple costo)
test_verifyB_false = verifyB (domB_lowCost, solB_ok) -- False

-- Sección 1.2 A: generación y solución A

varsA :: [Var]
varsA = getVars domA1
-- getVars debería devolver ["x","y","z"] ordenado
test_getVars = sort varsA  -- ["x","y","z"]
-- getPossibleSolutionsA debería generar 2^3 = 8 asignaciones
test_getPossibleSolutionsA = length (getPossibleSolutionsA varsA) -- 8
-- solveA debería producir una solución que verifica el dominio
test_solveA = verifyA (domA1, solveA domA1)  -- True

-- Sección 1.2 B: generación y solución B
-- getPossibleSolutionsB debería generar 2^2 = 4 subconjuntos
test_getPossibleSolutionsB = length (getPossibleSolutionsB ps) -- 4
-- solveB debería producir una solución válida (cumple verifyB)
test_solveB = verifyB (domB, solveB domB) -- True

-- Tests verify3SatTR
-- con acumulador inicial True, devuelve True
test_verify3SatTR_true = verify3SatTR domA1 solA1 True  -- True
-- con acumulador inicial False, devuelve False
test_verify3SatTR_false = verify3SatTR domA1 solA1 False -- False

tests_to_verify_b :: [(Bool, Bool)]
tests_to_verify_b = [
    (test_verifyCostB_true, True),
    (test_verifyCostB_false, False),
    (test_verifyBenefitsB_true, True),
    (test_verifyBenefitsB_false, False),
    (test_getGroupIndex_found == 0, True),
    (test_getGroupIndex_notfound == -1, True),
    (test_groupIsSatisfied_true, True),
    (test_groupIsSatisfied_false, False),
    (test_verifyB_true, True),
    (test_verifyB_false, False),
    (test_getPossibleSolutionsB == 4, True),
    (test_solveB, True)
  ]

tests_to_verify_a :: [(Bool, Bool)]
tests_to_verify_a = [
    (test_verifyTerm_true, True),
    (test_verifyTerm_false, False),
    (test_verifyPhi_true, True),
    (test_verifyPhi_false, False),
    (test_verifyA_true, True),
    (test_verifyA_false, False),
    (test_getVars == ["x","y","z"], True),
    (test_getPossibleSolutionsA == 8, True),
    (test_solveA, True)
  ]

verifyTests :: [(Bool, Bool)] -> Bool
verifyTests = foldr (\(a, b) -> (&&) (a == b)) True
