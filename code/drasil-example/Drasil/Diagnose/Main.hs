module Main (main) where

import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.Diagnose.Body (srs, printSetting)

main :: IO()
main = do
  gen (DocSpec SRS     "Diagnose_SRS") srs printSetting
  gen (DocSpec Website "Diagnose_SRS") srs printSetting
