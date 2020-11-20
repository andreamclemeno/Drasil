module Drasil.Diagnose.TMods (tMods) where

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (TheoryModel, tm)

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, s_2, second, mole, litre)
import Theory.Drasil (mkQuantDef)

import Drasil.Diagnose.Unitals
import Drasil.Diagnose.References (accelerationWiki, velocityWiki, hibbeler2004)

tMods :: [TheoryModel]
tMods = [rateTM]
--tMods = [accelerationTM, velocityTM]

rateTM :: TheoryModel
rateTM = tm (cw rateRC)
  [qw vRate, qw vLoadt, qw time] ([] :: [ConceptChunk]) [] [rateRel] []
  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "rate-elimination" []

rateRC :: RelationConcept
rateRC = makeRC "rateRC" (cn' " Average rate of elimination") EmptyS rateRel

rateRel :: Relation
rateRel = sy vRate $= deriv (sy vLoadt) time

------------

expElimTM :: TheoryModel
expElimTM = tm (cw expElimRC)
  [qw vLoadt, qw elimConst, qw vLoado, qw time] ([] :: [ConceptChunk]) [] [expElimRel] []
  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "expElim" []

expElimRC :: RelationConcept
expElimRC = makeRC "expElimRC" (cn' "vLoadt") EmptyS expElimRel

expElimRel :: Relation
expElimRel = sy vLoadt * sy elimConst $= deriv (sy vLoado) time

----------

viralelimTM :: TheoryModel
viralelimTM = tm (cw viralelimRC)
  [qw concentration, qw numberV, qw vol] ([] :: [ConceptChunk]) [] [viralelimRel] []
  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "viralelim" []

viralelimRC :: RelationConcept
viralelimRC = makeRC "viralelimRC" (cn' "concentration") EmptyS viralelimRel

viralelimRel :: Relation
viralelimRel = sy concentration $= sy numberV / sy vol

----------
