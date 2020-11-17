module Drasil.DiagnosisAIDs.Figures (figVirusbody) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/DiagnosisAIDs/"

figVirusbody :: LabelledContent
figVirusbody = llcc (makeFigRef "Virusbody") $ figWithWidth (S "The" +:+ phrase physicalSystem)
  (resourcePath ++ "figVirusbody.jpg") 70
