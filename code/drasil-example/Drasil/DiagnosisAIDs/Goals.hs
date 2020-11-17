module Drasil.DiagnosisAIDs.Goals (goals) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)

--import Drasil.Projectile.Concepts (projectile, target)

goals :: [ConceptInstance]
goals = []
--goals = [targetHit]

--targetHit :: ConceptInstance
--targetHit = cic "targetHit" 
--  (S "Determine if the" +:+ phrase projectile +:+ S "hits the" +:+. phrase target)
--  "targetHit" goalStmtDom