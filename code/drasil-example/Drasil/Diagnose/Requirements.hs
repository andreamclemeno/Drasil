
module Drasil.Diagnose.Requirements (funcReqs, nonfuncReqs, verifyOutput) where
import Language.Drasil
import Drasil.DocLang.SRS (datCon, propCorSol)
import Utils.Drasil


import Data.Drasil.Concepts.Documentation (funcReqDom, nonFuncReqDom)
import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation as Doc (assumption, code, datumConstraint,
  environment, funcReqDom, likelyChg, mg, mis, module_, nonFuncReqDom, output_,
  property, requirement, srs, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, thModel)

import Drasil.Diagnose.Assumptions


{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [verifyInput, calcValues, verifyOutput, outputValues]
--inputValues,
verifyInput, calcValues, verifyOutput, outputValues :: ConceptInstance
--inputValues,
--inputValues  = cic "inputValues"  inputValuesDesc  "Input-Values"        funcReqDom
verifyInput  = cic "verifyInput"  verifyInputDesc  "Verify-Input-Values" funcReqDom
calcValues   = cic "calcValues"   calcValuesDesc   "Calculate-Values"    funcReqDom
verifyOutput = cic "verifyOutput" verifyOutputDesc "Verify-Output"       funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values"       funcReqDom

--inputValuesDesc :: Sentence
--inputValuesDesc = foldlSent [S "Input two viral load concentrations taken on ",
--  S "two consecutive days"]
  
verifyInputDesc :: Sentence
verifyInputDesc = foldlSent [S "The software will ensure that the inputs are not  ",
  S "out of bounds and in accordance with the data constraints especially regarding the" +:+ makeRef2S initialInf +:+ S "and furthermore," +:+ makeRef2S alwaysElim :+: S ". If any inputs are",
  S "out of bounds, an error message is displayed"]
   
calcValuesDesc :: Sentence
calcValuesDesc = foldlSent [S "Software calculates the initial elimination rate of HIV virus and the predicted viral load after the chosen prediction period"]
  
verifyOutputDesc :: Sentence
verifyOutputDesc = foldlSent [S "The output values will be cross referenced with ",
  S "the result constraints. This requirement is related to the assumptions:" +:+ makeRef2S initialInf +:+ S "and",makeRef2S alwaysElim]
  
outputValuesDesc :: Sentence
outputValuesDesc = foldlSent [S "Output related requirements including the elimination rate and predicted viral load to satisfy the goals of Diagnose"]
  
{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [verifiable, understandable, reusable, maintainable, portable]

verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  S "The Drasil generated python", phrase code, S "is tested using static and dynamic code analysis, linting, unit and system testing and continuous integration methods outlined in the Verification ",
  S "and Validation plan of Diagnose"]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "The", phrase code, S "will be generated using the Drasil Framework into modules to address each requirement"]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  S "The code will be created using Drasil. The framework has data libraries of scientific theories, concepts, quantities and units commonly used in scientific problems. The created knowledge libraries of Diagnose can be reused as required"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in traceability matrices throughout the documentation of Diagnose to measure maintainability. The traceability will be measured by ensuring the connections are recorded and identifiable"]) "Maintainable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The", phrase code, S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom

