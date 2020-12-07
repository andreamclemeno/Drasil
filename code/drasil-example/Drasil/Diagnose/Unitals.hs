module Drasil.Diagnose.Unitals where 

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.IdeaDicts
import Theory.Drasil --(mkQuantDef)
import Utils.Drasil
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  requirement, srs, typUnc)

import Data.Drasil.SI_Units (metre, s_2,m_3, second, day, mole, millilitre)
import Data.Drasil.Concepts.Math
import Control.Lens ((^.))
import Data.Drasil.Constraints (gtZeroConstr)

acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]
  
symbols :: [QuantityDict]  
symbols =  map qw [time, ttesting, tpredict, vLoad, vol, numberV, vLoado,vLoadt,vRate, elimConst, predictedVL]  
--add if using (map qw unitless)

inputs :: [QuantityDict]  
inputs =  map qw [vLoado, vLoadt, ttesting, tpredict] 

outputs :: [QuantityDict]  
outputs =  map qw [elimConst, predictedVL] 

defSymbols :: [DefinedQuantityDict]
defSymbols = map dqdWr unitsymbs

unitsymbs :: [UnitaryConceptDict]
unitsymbs = map ucw tMUC 

constants :: [QDefinition]
constants = []

--ttestingU, tpredictU,
----------
vLoadU, vLoadtU, vLoadoU, vRateU, elimConstU, predictedVLU:: UnitDefn

--ttestingU       = newUnit "time between primary viral load test and secondary viral load test" $ day
--tpredictU    = newUnit "time between initial viral load test and chosen prediction" $ day
vLoadU           = newUnit "viral load"                 $ mole /: millilitre
vLoadtU         = newUnit "secondary viral load at t test" $ mole /: millilitre 
vLoadoU         = newUnit "initial viral load"        $ mole /: millilitre 
vRateU          = newUnit "rate of change of viral load" $ mole /$ (millilitre *: day)
elimConstU      = newUnit "first-order rate constant"    $ day ^: (-1)
predictedVLU    = newUnit "predicted viral load after chosen prediction period" $ mole /: millilitre

----------

tMCC :: [ConceptChunk]
tMCC = [timec, ttestingc, tpredictc, vLoadc, numberVc, volc, vLoadtc, vLoadoc, vRatec, elimConstc, predictedVLc]


timec = dcc "time" (cn' "time")
  "the indefinite continued progress of existence and events in the past, present, and future regarded as a whole"
  
ttestingc = dcc "timeTest" (cn' "time at secondary test")
  "the indefinite continued progress of existence and events in the past, present, and future regarded as a whole"

tpredictc = dcc "timePredict" (cn' "chosen prediction period")
  "the amount of time between the events of the initial time and time of viral load prediction"
  
vLoadc = dccWDS "vLoad" (cn' "viral load")
  (S "the amount of virions in a volumetric area")

numberVc = dcc "numberV" (cn' "number of virions")
  "the number of virions in the body"

volc = dcc "vol" (cn' "volume")
  "the amount of three-dimensional space associated with the body"

elimConstc = dcc "elimConst" (cn' "elimination constant")
  "a constant that describes the changing number of virions"

----- variants of viral load

vLoadtc = dccWDS "vLoadt" (cn "viral load at time t")
  (S "the amount of copies (in mol) in a volumetric area at time t")
  

vLoadoc = dccWDS "vLoado" (cn "initial viral load")
  (S "the initial amount of copies (in mol) in a volumetric area")
  
vRatec = dccWDS "vRate" (cn' "rate of change of the viral load")
  (S "the rate of change of the viral load")
  
predictedVLc = dccWDS "predictedVL" (cn "predicted viral load after 30 days")
  (S "the amount of copies (in mol) in a volumetric area after 30 days")
  


----------

tMUC :: [UnitalChunk]
tMUC = [time, ttesting, tpredict, vLoad, vol, numberV, vLoado, vLoadt, vRate]

time, ttesting, tpredict, vol, vLoad, numberV, vLoado, vLoadt, vRate, elimConst, predictedVL :: UnitalChunk


time                 = uc timec lT day
ttesting             = uc ttestingc (sub (lT) (Label "t"))  day
tpredict             = uc tpredictc (sub (lT) (Label "p")) day
vol                  = uc volc cV millilitre
numberV              = uc numberVc lN  mole
vLoad                = uc vLoadc cN  vLoadU
vLoadt               = uc vLoadtc (sub (cN) (Label "t"))  vLoadtU
vLoado               = uc vLoadoc (sub (cN) (Label "o"))  vLoadoU
vRate                = uc vRatec lR vRateU
elimConst	     = uc elimConstc lK elimConstU
predictedVL          = uc predictedVLc (sub (cN) (Label "p"))  predictedVLU



-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

vLoadtCons, vLoadoCons, ttestingCons, tpredictCons, elimConstCons, predictedVLCons :: ConstrConcept

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt)
  [vLoadtCons, vLoadoCons, ttestingCons, tpredictCons]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) 
  [elimConstCons, predictedVLCons]


vLoadtCons      = constrained' vLoadt      [physc $ Bounded (Exc,0) (Exc, sy vLoado)] (dbl 5000000)
vLoadoCons      = constrained' vLoado      [gtZeroConstr] (dbl 10000000)
ttestingCons    = constrained' ttesting    [physc $ Bounded (Exc,0) (Exc, sy tpredict)] (dbl 1)
tpredictCons    = constrained' tpredict    [gtZeroConstr] (dbl 30)

elimConstCons   = constrained' elimConst   [gtZeroConstr] (dbl 0.02)
predictedVLCons = constrained' predictedVL [gtZeroConstr] (dbl 200000)







