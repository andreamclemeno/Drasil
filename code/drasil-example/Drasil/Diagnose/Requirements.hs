--module Drasil.Diagnose.Requirements (nonfuncReqs) where
module Drasil.Diagnose.Requirements (funcReqs, nonfuncReqs) where
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


{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [inputValues, verifyInput, calcValues, verifyOutput, outputValues]

inputValues, verifyInput, calcValues, verifyOutput, outputValues :: ConceptInstance

inputValues  = cic "inputValues"  inputValuesDesc  "Input-Values"        funcReqDom
verifyInput  = cic "verifyInput"  verifyInputDesc  "Verify-Input-Values" funcReqDom
calcValues   = cic "calcValues"   calcValuesDesc   "Calculate-Values"    funcReqDom
verifyOutput = cic "verifyOutput" verifyOutputDesc "Verify-Output"       funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values"       funcReqDom

inputValuesDesc :: Sentence
inputValuesDesc = foldlSent [S "Input two viral load concentrations taken on ",
  S "two consecutive days"]
  
verifyInputDesc :: Sentence
verifyInputDesc = foldlSent [S "The software will ensure that the inputs are not  ",
  S "out of bounds and in accordance with the data constraints. If any inputs are",
  S "out of bounds, an error message is displayed"]
   
calcValuesDesc :: Sentence
calcValuesDesc = foldlSent [S "Software calculates the viral load at 30 days and ",
  S "the probability of progression to AIDs after 3 years"]
  
verifyOutputDesc :: Sentence
verifyOutputDesc = foldlSent [S "The output values will be cross referenced  with ",
  S "the result constraints"]
  
outputValuesDesc :: Sentence
outputValuesDesc = foldlSent [S "Output related requirements "]


  
{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

correct :: ConceptInstance
correct = cic "correctness" (foldlSent [
  S "The outputs have to adhere to the output properties in the output ",
  S "constraints"]) "Correctness" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  S "The", phrase code, S "is tested with complete verification ",
  S "and validation plan"]) "Verifiable" nonFuncReqDom

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

