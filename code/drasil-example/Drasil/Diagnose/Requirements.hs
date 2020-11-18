module Drasil.Diagnose.Requirements (nonfuncReqs) where

--(funcReqs, nonfuncReqs) where
import Language.Drasil
import Drasil.DocLang.SRS (datCon, propCorSol)
import Utils.Drasil

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (assumption, code, datumConstraint,
  environment, funcReqDom, likelyChg, mg, mis, module_, nonFuncReqDom, output_,
  property, requirement, srs, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, thModel)

--import Drasil.Projectile.IMods (landPosIM, messageIM, offsetIM, timeIM)
--import Drasil.Projectile.Unitals (flightDur, landPos, message, offset)

--{--Functional Requirements--}

--funcReqs :: [ConceptInstance]
--funcReqs = [verifyInVals, calcValues, outputValues]
--
--verifyInVals, calcValues, outputValues :: ConceptInstance
--
--verifyInVals = cic "verifyInVals" verifyParamsDesc "Verify-Input-Values" funcReqDom
--calcValues   = cic "calcValues"   calcValuesDesc   "Calculate-Values"    funcReqDom
--outputValues = cic "outputValues" outputValuesDesc "Output-Values"       funcReqDom
--
--verifyParamsDesc, calcValuesDesc, outputValuesDesc :: Sentence
--verifyParamsDesc = foldlSent [S "Check the entered", plural inValue,
--  S "to ensure that they do not exceed the", plural datumConstraint,
--  S "mentioned in" +:+. makeRef2S (datCon ([]::[Contents]) ([]::[Section])), 
--  S "If any of the", plural inValue, S "are out of bounds" `sC`
--  S "an", phrase errMsg, S "is displayed" `andThe` plural calculation, S "stop"]
--calcValuesDesc = foldlSent [S "Calculate the following" +: plural value,
--  foldlList Comma List [
--    ch flightDur +:+ sParen (S "from" +:+ makeRef2S timeIM),
--    ch landPos   +:+ sParen (S "from" +:+ makeRef2S landPosIM),
--    ch offset    +:+ sParen (S "from" +:+ makeRef2S offsetIM),
--    ch message   +:+ sParen (S "from" +:+ makeRef2S messageIM)
--  ]]
--outputValuesDesc = foldlSent [atStart output_, ch message,
--  sParen (S "from" +:+ makeRef2S messageIM) `sAnd` ch offset,
--  sParen (S "from" +:+ makeRef2S offsetIM)]
--
{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  plural output_ `ofThe'` phrase code, S "have the",
  plural property, S "described in", makeRef2S (propCorSol [] [])
  ]) "Correct" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  S "The", phrase code, S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "The", phrase code, S "is modularized with complete",
  phrase mg `sAnd` phrase mis]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  S "The", phrase code, S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix, S "in the", getAcc srs `sAnd` phrase mg]) "Maintainable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The", phrase code, S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom

