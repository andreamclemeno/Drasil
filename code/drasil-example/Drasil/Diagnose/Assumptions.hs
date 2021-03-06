module Drasil.Diagnose.Assumptions (assumptions, allProductive, alwaysElim, constConditions,
  constGrowth, constVolume, initialInf, neglectDrugs, neglectSick,
  proportional) where

import Language.Drasil
import Utils.Drasil

import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation (assumpDom)

import Drasil.Diagnose.Concepts

  
-- alphabetical: allProductive, alwaysDecay, constConditions, constGrowth, constVolume, initialInf, neglectDrugs, 
--neglectSick, proportional
  
assumptions :: [ConceptInstance]
assumptions = [initialInf, constGrowth, constVolume, constConditions, allProductive, 
  alwaysElim, neglectDrugs, neglectSick, proportional] 


initialInf, constGrowth, constVolume, constConditions, allProductive,
  alwaysElim, neglectDrugs, neglectSick, 
  proportional :: ConceptInstance
initialInf         = cic "initialInf"        initialInfDesc         "initialInf"        assumpDom
constGrowth        = cic "constGrowth"       constGrowthDesc        "constGrowth"       assumpDom
constVolume        = cic "constVolume"       constVolumeDesc        "constVolume"       assumpDom
constConditions    = cic "constConditions"   constConditionsDesc    "constConditions"   assumpDom
allProductive      = cic "allProductive"     allProductiveDesc      "allProductive"     assumpDom
alwaysElim         = cic "alwaysElim"        alwaysElimDesc         "alwaysElim"        assumpDom
neglectDrugs       = cic "neglectDrugs"      neglectDrugsDesc       "neglectDrugs"      assumpDom
neglectSick        = cic "neglectSick"       neglectSickDesc        "neglectSick"       assumpDom
proportional       = cic "proportional"      proportionalDesc       "proportional"     assumpDom

initialInfDesc :: Sentence
initialInfDesc = S "Initial infection of HIV for patient is assumed"

constGrowthDesc :: Sentence
constGrowthDesc = S "The virions will invade uninfected cells at a constant rate."

constVolumeDesc :: Sentence
constVolumeDesc = S "The dimensions of the location associated with the infection remains constant"

constConditionsDesc :: Sentence
constConditionsDesc = S "Temperature of the location associated with the infection remains constant"

allProductiveDesc :: Sentence
allProductiveDesc = S "All viruses within the volume infect cells productively"

alwaysElimDesc :: Sentence
alwaysElimDesc = foldlSent[S "In accordance with", makeRef2S initialInf `sC` S "after viremia peak, no significant upward trends occur"]

neglectDrugsDesc :: Sentence
neglectDrugsDesc = foldlSent [S "The effect of antibiotic drugs or therapy on the elimination rate will be not be considered"]

neglectSickDesc :: Sentence
neglectSickDesc = foldlSent [S "With reference to", makeRef2S constConditions `sC` S "the effect of other infections on the elimination rate will be not be considered"]

proportionalDesc :: Sentence
proportionalDesc = S "The elimination of the virus is assumed to be proportional to the amount of viruses present"




