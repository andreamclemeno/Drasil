module Drasil.Diagnose.TMods (tMods, expElimTM) where

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (TheoryModel, tm)

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, s_2, second, mole, litre)
import Theory.Drasil (mkQuantDef)

import Drasil.Diagnose.Unitals
import Drasil.Diagnose.References (accelerationWiki, velocityWiki, hibbeler2004)

tMods :: [TheoryModel]
tMods = [expElimTM]

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
  [qw vRate, qw vLoado, qw elimConst, qw vLoad, qw time] ([] :: [ConceptChunk]) [] [expElimRel] []
  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "expElim" []

expElimRC :: RelationConcept
expElimRC = makeRC "expElimRC" (cn' "vLoad") EmptyS expElimRel

expElimRel :: Relation
expElimRel = sy vRate $= deriv (sy vLoad) time $= negate (sy elimConst * sy vLoado) 

----------

viralelimTM :: TheoryModel
viralelimTM = tm (cw viralelimRC)
  [qw vLoad, qw numberV, qw vol] ([] :: [ConceptChunk]) [] [viralelimRel] []
  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "viralelim" []

viralelimRC :: RelationConcept
viralelimRC = makeRC "viralelimRC" (cn' "concentration") EmptyS viralelimRel

viralelimRel :: Relation
viralelimRel = sy vLoad $= sy numberV / sy vol

----------
