module Drasil.Diagnose.TMods (tMods, expElimTM) where

import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, s_2, second, mole, litre)
import Theory.Drasil (DataDefinition, GenDefn, TheoryModel, ddNoRefs, gd, 
  mkQuantDef, tmNoRefs, tm)

import Data.Drasil.Concepts.Documentation (body, component, constant, material_,
  value)
  

import Drasil.Diagnose.Assumptions(allProductive, proportional)
import Drasil.Diagnose.Unitals
import Drasil.Diagnose.References (accelerationWiki, velocityWiki, hibbeler2004)

tMods :: [TheoryModel]
tMods = [expElimTM]

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


  
