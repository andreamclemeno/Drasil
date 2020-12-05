module Main (main) where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), AuxFile(..), Visibility(..),
  defaultChoices)
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.Diagnose.Body (si, srs, printSetting)
import Drasil.Diagnose.ModuleDefs (allMods)

code :: CodeSpec
code = codeSpec si choices allMods

choices :: Choices
choices = defaultChoices {
  lang = [Python], -- Cpp, CSharp, Java, Swift
  modularity = Modular Separated,
  impType = Program,
  logFile = "log.txt",
  logging = [LogVar, LogFunc],
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Exception,
  inputStructure = Bundled,
  constStructure = Inline,
  constRepr = Const,
  auxFiles = [SampleInput "../../datafiles/Diagnose/sampleInput.txt", ReadME] 
}
  
main :: IO()
main = do
  gen (DocSpec SRS     "Diagnose_SRS") srs printSetting
  gen (DocSpec Website "Diagnose_SRS") srs printSetting
  genCode choices code
