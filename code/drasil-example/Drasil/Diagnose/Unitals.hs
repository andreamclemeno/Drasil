module Drasil.Diagnose.Unitals where 

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (mkQuantDef)
import Utils.Drasil

import Data.Drasil.SI_Units --(joule, metre, newton, pascal, radian, s_2, second, mole, litre)
import Data.Drasil.Concepts.Math
import Control.Lens ((^.))

symbols :: [QuantityDict]  
symbols =  map qw [acceleration, time, velocity, concentration, vol, numberV, vLoado,vLoadt,vRate] ++ map qw unitless

defSymbols :: [DefinedQuantityDict]
defSymbols = map dqdWr unitsymbs

unitsymbs :: [UnitaryConceptDict]
unitsymbs = map ucw tMUC 


----------
accelU, velU, concU, vLoadtU, vLoadoU, vRateU :: UnitDefn

accelU          = newUnit "acceleration"                 $ metre /: s_2
velU            = newUnit "velocity"                     $ metre /: second
concU           = newUnit "concentration"                $ mole /: litre
vLoadtU         = newUnit "concentration at t"           $ mole /: litre 
vLoadoU         = newUnit "initial concentration"        $ mole /: litre 
vRateU          = newUnit "rate of change of viral load" $ mole /$ (litre *: second)

----------

tMCC :: [ConceptChunk]
tMCC = [accelerationc, timec, velocityc, concentrationc, numberVc, volc, vLoadtc, vLoadoc, vRatec]

accelerationc = dccWDS "acceleration" (cn' "acceleration")
  (S "the rate of change of a body's" +:+ phrase velocity)

timec = dcc "time" (cn' "time")
  "the indefinite continued progress of existence and events in the past, present, and future regarded as a whole"

velocityc = dccWDS "velocity" (cnIES "velocity")
  (S "the rate of change of a body's position")
  
concentrationc = dccWDS "concentration" (cn' "concentration")
  (S "the amount of a substance in a volumetric area")

numberVc = dcc "numberV" (cn' "numberV")
  "the number of virions in the body"

volc = dcc "vol" (cn' "vol")
  "the amount of three-dimensional space associated with the body"
  
vLoadtc = dccWDS "concentration" (cn' "concentration")
  (S "the amount of a substance in a volumetric area at time t")

vLoadoc = dccWDS "concentration" (cn' "concentration")
  (S "the initial amount of a substance in a volumetric area")
  
vRatec = dccWDS "vRate" (cn' "vRate")
  (S "the rate of change of the viral load")
  

----------

tMUC :: [UnitalChunk]
tMUC = [acceleration, time, velocity, concentration, vol, numberV, vLoado, vLoadt, vRate]

acceleration, time, velocity, vol, concentration, numberV, vLoado, vLoadt, vRate :: UnitalChunk

acceleration         = uc accelerationc (vec lA) accelU
time                 = uc timec lT second
velocity             = uc velocityc (vec lV) velU
vol                  = uc volc cV litre
numberV              = uc numberVc lN  mole
concentration        = uc concentrationc cC  concU
vLoadt               = uc concentrationc cA  vLoadtU
vLoado               = uc concentrationc cB  vLoadoU
vRate                = uc vRatec lR vRateU

-------------

unitless :: [QuantityDict]
unitless = [elimConst] 

elimConst :: QuantityDict

elimConst = vc "elimConst" (nounPhraseSP "dimensionless constant") (lK) Real







