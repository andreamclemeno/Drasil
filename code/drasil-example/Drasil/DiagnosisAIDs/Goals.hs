module Drasil.DiagnosisAIDs.Goals (goals) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)

--import Drasil.Projectile.Concepts (projectile, target)

goals :: [ConceptInstance]
goals = []
--goals = [determine-clearance-rate, predict-VL-30]

--targetHit :: ConceptInstance
--targetHit = cic "targetHit" 
--  (S "Determine if the" +:+ phrase projectile +:+ S "hits the" +:+. phrase target)
--  "targetHit" goalStmtDom

--determine-clearance-rate :: ConceptInstance
--determine-clearance-rate = cic "determine-clearance-rate" 
--  (S "Determine the" +:+ phrase clearance rate +:+ S "of the HIV Virus due to immune response.")
--  "determine-clearance-rate" goalStmtDom

--predict-VL-30 :: ConceptInstance
--predict-VL-30 = cic "predict-VL-30" 
--  (S "Determine the viral load at 30 days.")
--  "predict-VL-30" goalStmtDom

-- goalsInputs :: [Sentence]
-- goalsInputs = [phrase diagnosisAIDstitle `ofThe` diagnosisAIDstitle ]-}