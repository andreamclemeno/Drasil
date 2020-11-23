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
symbols =  map qw [time, vLoad, vol, numberV, vLoado,vLoadt,vRate, elimConst, predictedVL]  
--add if using (map qw unitless)

inputs :: [QuantityDict]  
inputs =  map qw [vLoado, vLoadt] 

outputs :: [QuantityDict]  
outputs =  map qw [elimConst, predictedVL] 

defSymbols :: [DefinedQuantityDict]
defSymbols = map dqdWr unitsymbs

unitsymbs :: [UnitaryConceptDict]
unitsymbs = map ucw tMUC 

constants :: [QDefinition]
constants = []


----------
vLoadU, vLoadtU, vLoadoU, vRateU, elimConstU, predictedVLU:: UnitDefn

vLoadU           = newUnit "viral load"                $ copies /: millilitre
vLoadtU         = newUnit "viral load at t"           $ copies /: millilitre 
vLoadoU         = newUnit "initial viral load"        $ copies /: millilitre 
vRateU          = newUnit "rate of change of viral load" $ copies /$ (millilitre *: second)
elimConstU      = newUnit "first-order rate constant"    $ second ^: (-1)
predictedVLU    = newUnit "predicted viral load after 30 days" $ copies /: millilitre

----------

tMCC :: [ConceptChunk]
tMCC = [timec, vLoadc, numberVc, volc, vLoadtc, vLoadoc, vRatec, elimConstc, predictedVLc]


timec = dcc "time" (cn' "time")
  "the indefinite continued progress of existence and events in the past, present, and future regarded as a whole"
  
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
  (S "the amount of copies in a volumetric area at time t")
  

vLoadoc = dccWDS "vLoado" (cn "initial viral load")
  (S "the initial amount of copies in a volumetric area")
  
vRatec = dccWDS "vRate" (cn' "rate of change of the viral load")
  (S "the rate of change of the viral load")
  
predictedVLc = dccWDS "predictedVL" (cn "predicted viral load after 30 days")
  (S "the amount of copies in a volumetric area after 30 days")
  


----------

tMUC :: [UnitalChunk]
tMUC = [time, vLoad, vol, numberV, vLoado, vLoadt, vRate]

time, vol, vLoad, numberV, vLoado, vLoadt, vRate, elimConst, predictedVL :: UnitalChunk


time                 = uc timec lT second
vol                  = uc volc cV millilitre
numberV              = uc numberVc lN  copies
vLoad                = uc vLoadc cN  vLoadU
vLoadt               = uc vLoadtc (sub (cN) (Label "t"))  vLoadtU
vLoado               = uc vLoadoc (sub (cN) (Label "o"))  vLoadoU
vRate                = uc vRatec lR vRateU
elimConst	     = uc elimConstc lLambda elimConstU
predictedVL          = uc predictedVLc (sub (cN) (Label "p"))  vLoadoU



-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

vLoadtCons, vLoadoCons, elimConstCons, predictedVLCons :: ConstrConcept

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt)
  [vLoadtCons, vLoadoCons]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) 
  [elimConstCons, predictedVLCons]


vLoadtCons      = constrained' vLoadt      [gtZeroConstr] (dbl 5000000)
vLoadoCons      = constrained' vLoado      [gtZeroConstr] (dbl 10000000)
elimConstCons   = constrained' elimConst   [gtZeroConstr] (dbl 0.02)
predictedVLCons = constrained' predictedVL [gtZeroConstr] (dbl 200000)







