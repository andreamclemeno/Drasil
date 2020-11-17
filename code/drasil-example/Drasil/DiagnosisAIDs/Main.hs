module Main (main) where

import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.DiagnosisAIDs.Body (srs, printSetting)

main :: IO()
main = do
  gen (DocSpec SRS     "DiagnosisAIDs_SRS") srs printSetting
  gen (DocSpec Website "DiagnosisAIDs_SRS") srs printSetting
