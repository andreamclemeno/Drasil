module Drasil.Diagnose.Goals (goals) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)

import Drasil.Diagnose.Concepts (elimination)

--goals :: [ConceptInstance]
--goals = [targetHit]
--
--targetHit :: ConceptInstance
--targetHit = cic "targetHit" 
--  (S "Determine if the hits the")
--  "targetHit" goalStmtDom
  
-------------------------
goals :: [ConceptInstance]
goals = [detElimrate, predictVL30]

detElimrate :: ConceptInstance
detElimrate = cic "detElimrate" 
  (S "Determine the elimination rate of the HIV Virus due to immune response.")
  "detElimrate" goalStmtDom
  
--  (S "Determine the" +:+ phrase elimination +:+ S "rate of the HIV Virus due to immune response.")
--  "detElimrate" goalStmtDom

predictVL30 :: ConceptInstance
predictVL30 = cic "predictVL30" 
  (S "Determine the viral load at 30 days.")
  "predictVL30" goalStmtDom
  
--  (S "Determine the viral load at 30 days.")
--  "predictVL30" goalStmtDom

--goalsInputs :: [Sentence]
--goalsInputs = [phrase diagnosisAIDstitle `ofThe` diagnosisAIDstitle ]-}