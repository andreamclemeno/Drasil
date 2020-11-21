module Drasil.Diagnose.Figures (figVirusinbody) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (physicalSystem)

resourcePath :: String
resourcePath = "../../../datafiles/Diagnose/"

figVirusinbody :: LabelledContent
figVirusinbody = llcc (makeFigRef "Virus") $ figWithWidth (S "The" +:+ phrase physicalSystem)
  (resourcePath ++ "Virusinbody.JPG") 70