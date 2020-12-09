module Drasil.Diagnose.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed)

defs :: [ConceptChunk]
defs = [virus, viralloaddef, replication, productivity, infectedcells, helperTcell, immuneresponse, elimination, viremiapeak, aids, diagnosis, progression]

diagnoseTitle :: CI
diagnoseTitle = commonIdeaWithDict "diagnoseTitle" (pn "Diagnose") "Diagnose" [physics]

---

virus, viralloaddef, replication, productivity, infectedcells, helperTcell, immuneresponse, elimination, viremiapeak, aids, diagnosis, progression :: ConceptChunk
virus          = dcc "virus"          (nounPhraseSP "virus")          "Submicroscopic parasites that infect cells"
viralloaddef   = dcc "viral load"     (nounPhraseSP "viral load")     ("The concentration of HIV virus at a " ++
                                                                      "point in time")
infectedcells  = dcc "infected cells" (nounPhraseSP "infected cells") ("Cells that interact with the virus " ++
                                                                      "replicate into cells altered by the virus")
helperTcell    = dcc "Helper T cell"  (nounPhraseSP "Helper T cell")  ("Cells of the immune system that " ++
                                                                      "neutralize infected cells")
elimination    = dcc "elimination"    (nounPhraseSP "elimination")    "Physical quantity undergoing a decline in amount"
aids           = dcc "AIDs"           (nounPhraseSP "AIDs")           ("Acquired Immunodeficiency Syndrome develops " ++
                                                                      "from an increase in HIV viral load to the " ++
                                                                      "extent where T cell count decreases to " ++
                                                                      "under 200 mol/L")
diagnosis      = dcc "diagnosis"      (nounPhraseSP "diagnosis")      ("The determination of a patient's condition " ++
                                                                      "reached by a healthcare professional")
progression    = dcc "progression"    (nounPhraseSP "progression")    "The development towards a more advanced stage"


replication = dcc "Replication"          (nounPhraseSP "Replication")          "The process where infected cells rapidly reproduce genetic material of the virus rather than its normal process of reproducing itself"

productivity = dcc "Productivity"          (nounPhraseSP "Productivity")          " Cells can be productive and non-productive. Productive means that infection occurs. The HIV-1 virus needs to interact with a cell to be productive and replicate"

immuneresponse = dcc "Immune Response"          (nounPhraseSP "Immune Response")          "The defensive reaction of the human body against harmful substances like the HIV-1 virus"

viremiapeak = dcc "Viremia Peak"          (nounPhraseSP "Viremia Peak")          "The peak of viral load after the virus has entered the blood stream. There is a trend of elimination for viral load due to the body's immune response"








