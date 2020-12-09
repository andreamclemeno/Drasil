module Drasil.Diagnose.Goals (goals, detElimrate, predictVL30) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)

import Drasil.Diagnose.Concepts (elimination)


-------------------------
goals :: [ConceptInstance]
goals = [detElimrate, predictVL30]

detElimrate :: ConceptInstance
detElimrate = cic "detElimrate" 
  (S "Determine the elimination rate of the HIV virus due to immune response.")
  "detElimrate" goalStmtDom
  

predictVL30 :: ConceptInstance
predictVL30 = cic "predictVL30" 
  (S "Predict viral load after chosen prediction period.")
  "predictVL30" goalStmtDom
  
--  (S "Predict viral load after chosen prediction period")
--  "predictVL30" goalStmtDom

--goalsInputs :: [Sentence]
--goalsInputs = [phrase diagnosisAIDstitle `ofThe` diagnosisAIDstitle ]-}