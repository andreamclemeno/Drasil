--module Drasil.Diagnose.TMods (tMods, expElim) where
--module Drasil.Diagnose.TMods (tMods, postViremia, expElim) where

import Language.Drasil
import Language.Drasil.Code (relToQD)
import Theory.Drasil (TheoryModel, tm)
import Database.Drasil (cdb)
import Drasil.Diagnose.References (accelerationWiki, velocityWiki, hibbeler2004) -------------- temp

import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties(amta, amtb)
import Drasil.Diagnose.Unitals (detPostViremia, vLoada, vLoadb, elimconst)


tMods :: [TheoryModel]
tMods = [expElim]
--tMods = [postViremia, expElim]
-------------------------------

expElim :: TheoryModel
expElim = tm (cw expElimRC)
  [qw amta, qw amtb, qw elimconst, qw time] ([] :: [ConceptChunk]) [] [expElimRel] []
  [makeCite accelerationWiki] "expElim" []

expElimRC :: RelationConcept
expElimRC = makeRC "expElimRC" (cn' "expElim") EmptyS expElimRel

expElimRel :: Relation
expElimRel = sy amtb $= sy amta * sy elimconst --- change to Nb = Na *(e^(-elimconst*time))

------------------------------------------------------------------
--import Drasil.Diagnose.IMods (symb)
--import Drasil.Diagnose.Unitals (detPostViremia, vLoada, vLoadb)
--import Drasil.Diagnose.Symbols (thisSymbols)
------------------------------------------------------------------

--postViremia :: TheoryModel
--postViremia = tm (cw postViremiaRC)
--   [qw detPostViremia, qw vLoada, qw vLoadb] ([] :: [ConceptChunk])
--   [] [sy detPostViremia $= sy vLoada $> sy vLoadb] [] [makeCite accelerationWiki] 
--   "postViremia" [postViremiaDesc]

--postViremia = tm (cw postViremiaRC)
--   [qw detPostViremia, qw vLoada, qw vLoadb] ([] :: [ConceptChunk])
--   [relToQD postViremiaRC] [sy detPostViremia $= sy vLoada $> sy vLoadb] [] [makeCite accelerationWiki] 
--   "postViremia" [postViremiaDesc
                          
--postViremiaRC :: RelationConcept
--postViremiaRC = makeRC "HIVelim" (nounPhraseSP "HIV elimination phase")
--  postViremiaDesc (sy detPostViremia $= sy vLoada $> sy vLoadb)
  
--postViremiaRel :: Relation
--postViremiaRel = sy detPostViremia $= sy vLoada $> sy vLoadb

--postViremiaDesc :: Sentence
--postViremiaDesc = tModDesc detPostViremia

--tModDesc :: QuantityDict -> Sentence
--tModDesc main = S "If" +:+. (ch main `sC` S "the virus is in the elimination phase")
  

-------------------------------

--expElim :: TheoryModel
--expElim = tm (cw expElimRC)
--  [qw acceleration, qw velocity, qw time] ([] :: [ConceptChunk]) [] [expElimRel] []
--  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "expElim" []
--
--expElimRC :: RelationConcept
--expElimRC = makeRC "expElimRC" (cn' "expElim") EmptyS expElimRel
--
--expElimRel :: Relation
--expElimRel = sy acceleration $= deriv (sy velocity) time


--------------PROJECTILE EXAMPLE

--module Drasil.Diagnose.TMods (tMods, accelerationTM, velocityTM) where
--
--import Language.Drasil
--import Theory.Drasil (TheoryModel, tm)
--import Data.Drasil.Quantities.Physics (acceleration, position, time, velocity)
--
--import Drasil.Diagnose.References (accelerationWiki, velocityWiki, hibbeler2004)
--
--tMods :: [TheoryModel]
--tMods = [accelerationTM, velocityTM]
--------------------------------------------------------
--accelerationTM :: TheoryModel
--accelerationTM = tm (cw accelerationRC)
--  [qw acceleration, qw velocity, qw time] ([] :: [ConceptChunk]) [] [accelerationRel] []
--  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "acceleration" []
--
--accelerationRC :: RelationConcept
--accelerationRC = makeRC "accelerationRC" (cn' "acceleration") EmptyS accelerationRel
--
--accelerationRel :: Relation
--accelerationRel = sy acceleration $= deriv (sy velocity) time

------------

--velocityTM :: TheoryModel
--velocityTM = tm (cw velocityRC)
--  [qw velocity, qw position, qw time] ([] :: [ConceptChunk]) [] [velocityRel] []
--  [makeCite velocityWiki, makeCiteInfo hibbeler2004 $ Page [6]] "velocity" []

--velocityRC :: RelationConcept
--velocityRC = makeRC "velocityRC" (cn' "velocity") EmptyS velocityRel

--velocityRel :: Relation
--velocityRel = sy velocity $= deriv (sy position) time
