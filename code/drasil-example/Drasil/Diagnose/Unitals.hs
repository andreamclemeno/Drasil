module Drasil.Diagnose.Unitals where 

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.IdeaDicts
import Theory.Drasil --(mkQuantDef)
import Utils.Drasil
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  requirement, srs, typUnc)

import Data.Drasil.SI_Units (copies, metre, s_2,m_3, second, mole, millilitre)
import Data.Drasil.Concepts.Math
import Control.Lens ((^.))
import Data.Drasil.Constraints (gtZeroConstr)

acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]
  
symbols :: [QuantityDict]  
symbols =  map qw [acceleration, time, velocity, vLoad, vol, numberV, vLoado,vLoadt,vRate, elimConst]  
--add if using (map qw unitless)

inputs :: [QuantityDict]  
inputs =  map qw [vLoado, vLoadt] 

outputs :: [QuantityDict]  
outputs =  map qw [elimConst] 

defSymbols :: [DefinedQuantityDict]
defSymbols = map dqdWr unitsymbs

unitsymbs :: [UnitaryConceptDict]
unitsymbs = map ucw tMUC 


----------
accelU, velU, vLoadU, vLoadtU, vLoadoU, vRateU, elimConstU :: UnitDefn

accelU          = newUnit "acceleration"                 $ metre /: s_2
velU            = newUnit "velocity"                     $ metre /: second
vLoadU           = newUnit "viral load"                $ copies /: millilitre
vLoadtU         = newUnit "viral load at t"           $ copies /: millilitre 
vLoadoU         = newUnit "initial viral load"        $ copies /: millilitre 
vRateU          = newUnit "rate of change of viral load" $ copies /$ (millilitre *: second)
elimConstU      = newUnit "first-order rate constant"    $ second ^: (-1)

----------

tMCC :: [ConceptChunk]
tMCC = [accelerationc, timec, velocityc, vLoadc, numberVc, volc, vLoadtc, vLoadoc, vRatec, elimConstc]

accelerationc = dccWDS "acceleration" (cn' "acceleration")
  (S "the rate of change of a body's" +:+ phrase velocity)

timec = dcc "time" (cn' "time")
  "the indefinite continued progress of existence and events in the past, present, and future regarded as a whole"

velocityc = dccWDS "velocity" (cnIES "velocity")
  (S "the rate of change of a body's position")
  
vLoadc = dccWDS "vLoad" (cn' "vLoad")
  (S "the amount of a substance in a volumetric area")

numberVc = dcc "numberV" (cn' "numberV")
  "the number of virions in the body"

volc = dcc "vol" (cn' "vol")
  "the amount of three-dimensional space associated with the body"

elimConstc = dcc "elimConst" (cn' "elimConst")
  "a constant that describes the changing number of virions"

----- variants of viral load

vLoadtc = dccWDS "vLoadt" (cn "viral load at time t")
  (S "the amount of a substance in a volumetric area at time t")
  

vLoadoc = dccWDS "vLoado" (cn "initial viral load")
  (S "the initial amount of a substance in a volumetric area")
  
vRatec = dccWDS "vRate" (cn' "vRate")
  (S "the rate of change of the viral load")
  


----------

tMUC :: [UnitalChunk]
tMUC = [acceleration, time, velocity, vLoad, vol, numberV, vLoado, vLoadt, vRate]

acceleration, time, velocity, vol, vLoad, numberV, vLoado, vLoadt, vRate, elimConst :: UnitalChunk

acceleration         = uc accelerationc (vec lA) accelU
time                 = uc timec lT second
velocity             = uc velocityc (vec lV) velU
vol                  = uc volc cV millilitre
numberV              = uc numberVc lN  copies
vLoad                = uc vLoadc cN  vLoadU
vLoadt               = uc vLoadtc (sub (cN) (Label "t"))  vLoadtU
vLoado               = uc vLoadoc (sub (cN) (Label "o"))  vLoadoU
vRate                = uc vRatec lR vRateU
elimConst	     = uc elimConstc lLambda elimConstU



-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

vLoadtCons, vLoadoCons, elimConstCons :: ConstrConcept

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt)
  [vLoadtCons, vLoadoCons]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) 
  [elimConstCons]


vLoadtCons    = constrained' vLoadt    [gtZeroConstr] (dbl 5000000)
vLoadoCons    = constrained' vLoado    [gtZeroConstr] (dbl 10000000)
elimConstCons = constrained' elimConst [gtZeroConstr] (dbl 0.02)







