module Drasil.Diagnose.Changes (likelyChgs, unlikelyChgs) where

--A list of likely and unlikely changes for GlassBR

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (condition, goal, input_, likeChgDom,
  software, system, unlikeChgDom, value, variable)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.PhysicalProperties (flexure)

import Drasil.Diagnose.Assumptions
--import Drasil.GlassBR.Concepts (blastRisk, glaSlab, glass)
--import Drasil.GlassBR.Unitals (explosion, lite)

{--LIKELY CHANGES--}

likelyChgs :: [ConceptInstance]
likelyChgs = [incTimeFrame, moreInputs, moreOutputs]

incTimeFrame, moreInputs, moreOutputs :: ConceptInstance

incTimeFrame    = cic "incTimeFrame"     incTimeFrameDesc  "Increase-time-frame"       likeChgDom
moreInputs      = cic "moreInputs"       moreInputsDesc    "More-Inputs"               likeChgDom
moreOutputs     = cic "moreOutputs"      moreOutputsDesc   "More-Outputs"              likeChgDom


incTimeFrameDesc, moreInputsDesc, moreOutputsDesc :: Sentence

incTimeFrameDesc = foldlSent [(S "The"), phrase software,
  S "may be expanded to cover a wide range of time frames which is possible due to " +:+ makeRef2S proportional]

moreInputsDesc = foldlSent [(S "The"), phrase software,
  S "may be expanded to include more inputs from the user",
  S " to increase the accuracy of the output. This change may alter assumptions:" +:+ makeRef2S allProductive +:+ S "," +:+ makeRef2S neglectSick +:+ S "and" +:+ makeRef2S neglectDrugs]
  
moreOutputsDesc = foldlSent [(S "The"), phrase software,
  S "may be expanded to include more outputs",
  S " like a suggestion for therapy"]



{--UNLIKELY CHANGES--}

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [detElimRate, externalInput, inConstraints]

detElimRate, externalInput, inConstraints :: ConceptInstance

detElimRate    = cic "detElimRate"     detElimRateDesc      "Determine-elimination-rate"   unlikeChgDom
externalInput  = cic "externalInput"   externalInputDesc    "External-input"               unlikeChgDom
inConstraints  = cic "inConstraints"   inConstraintsDesc    "Unchanging-input-constraints" unlikeChgDom

detElimRateDesc, externalInputDesc, inConstraintsDesc :: Sentence

detElimRateDesc = foldlSent [(S "The goal of determining the elimination"),
  S "rate of the virus will not likely change"]

externalInputDesc = foldlSent [(S "There will always be a source of input "),
  S "data external to the software"]
  
inConstraintsDesc = foldlSent [(S "The input constraints will not "),
  S "likely change"]
  
  
  
  
  
  