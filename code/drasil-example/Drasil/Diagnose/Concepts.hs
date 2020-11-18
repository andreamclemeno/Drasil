module Drasil.Diagnose.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed)

--concepts :: [IdeaDict]
--concepts = map nw [landingPos, launch, launchAngle, launchSpeed, offset, targetPos]
--  ++ map nw defs

defs :: [ConceptChunk]
defs = [virus, viralload, infectedcells, helperTcell, elimination, aids, diagnosis, progression]

diagnoseTitle :: CI
diagnoseTitle = commonIdeaWithDict "diagnoseTitle" (pn "Diagnose") "Diagnose" [physics]

--duration, flightDur, landingPos, launch, launchAngle, launchSpeed, offset, targetPos :: NamedChunk
--duration   = nc "duration" (nounPhraseSP "duration")
--launch     = nc "launch"   (nounPhraseSP "launch") -- FIXME: Used as adjective
--offset     = nc "offset"   (nounPhraseSent $ S "distance between the" +:+ phrase targetPos `andThe` phrase landingPos)

--flightDur   = compoundNC (nc "flight"  (nounPhraseSP "flight" )) duration
--landingPos  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
--launchAngle = compoundNC launch angle
--launchSpeed = compoundNC launch speed
--targetPos   = compoundNC target position

---

virus, viralload, infectedcells, helperTcell, elimination, aids, diagnosis, progression :: ConceptChunk
virus          = dcc "virus"          (nounPhraseSP "virus")          "Submicroscopic parasites that infect cells"
viralload      = dcc "viral load"     (nounPhraseSP "viral load")     ("The concentration of HIV virus at a " ++
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







