module Drasil.DiagnosisAIDs.Assumptions (accelYGravity, accelXZero, cartSyst,
  assumptions, constAccel, gravAccelValue, launchOrigin, pointMass, 
  posXDirection, targetXAxis, timeStartZero, twoDMotion, yAxisGravity) where
 ---------------------------------------------------
--module Drasil.DiagnosisAIDs.Assumptions (accelYGravity, accelXZero, cartSyst,
--  assumptions, constAccel, gravAccelValue, launchOrigin, pointMass, 
--  posXDirection, targetXAxis, timeStartZero, twoDMotion, yAxisGravity) where
----------------------------------------------------------------

import Language.Drasil
import Utils.Drasil

import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation (assumpDom, value)
import Data.Drasil.Concepts.Math (cartesian, xAxis, xDir, yAxis, yDir)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (acceleration, collision, distance, gravity, time, twoD)

--import Drasil.DiagnosisAIDs.Concepts (launcher, projectile, target)

--import Drasil.Projectile.Concepts (launcher, projectile, target)

--assumptions :: [ConceptInstance]
--assumptions = [twoDMotion, cartSyst, yAxisGravity, launchOrigin, targetXAxis, 
--  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag, pointMass, 
--  freeFlight, neglectCurv, timeStartZero, gravAccelValue]

twoDMotion, cartSyst, yAxisGravity, launchOrigin, targetXAxis,
  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag,
  pointMass, freeFlight, neglectCurv, timeStartZero, 
  gravAccelValue :: ConceptInstance
twoDMotion      = cic "twoDMotion"      twoDMotionDesc      "twoDMotion"      assumpDom
cartSyst        = cic "cartSyst"        cartSystDesc        "cartSyst"        assumpDom
yAxisGravity    = cic "yAxisGravity"    yAxisGravityDesc    "yAxisGravity"    assumpDom
launchOrigin    = cic "launchOrigin"    launchOriginDesc    "launchOrigin"    assumpDom
targetXAxis     = cic "targetXAxis"     targetXAxisDesc     "targetXAxis"     assumpDom
posXDirection   = cic "posXDirection"   posXDirectionDesc   "posXDirection"   assumpDom
constAccel      = cic "constAccel"      constAccelDesc      "constAccel"      assumpDom
accelXZero      = cic "accelXZero"      accelXZeroDesc      "accelXZero"      assumpDom
accelYGravity   = cic "accelYGravity"   accelYGravityDesc   "accelYGravity"   assumpDom
neglectDrag     = cic "neglectDrag"     neglectDragDesc     "neglectDrag"     assumpDom
pointMass       = cic "pointMass"       pointMassDesc       "pointMass"       assumpDom
freeFlight      = cic "freeFlight"      freeFlightDesc      "freeFlight"      assumpDom
neglectCurv     = cic "neglectCurv"     neglectCurvDesc     "neglectCurv"     assumpDom
timeStartZero   = cic "timeStartZero"   timeStartZeroDesc   "timeStartZero"   assumpDom
gravAccelValue  = cic "gravAccelValue"  gravAccelValueDesc  "gravAccelValue" assumpDom


twoDMotionDesc :: Sentence
twoDMotionDesc = S "The" +:+ phrase projectile +:+ S "motion" `sIs` phrase twoD +:+. sParen (getAcc twoD)

cartSystDesc :: Sentence
cartSystDesc = S "A" +:+ (phrase cartesian `sIs` S "used") +:+. sParen (S "from" +:+ makeRef2S neglectCurv)

yAxisGravityDesc :: Sentence
yAxisGravityDesc = S "direction" `ofThe'` phrase yAxis `sIs` S "directed opposite to" +:+. phrase gravity

launchOriginDesc :: Sentence
launchOriginDesc = S "The" +:+. (phrase launcher `sIs` S "coincident with the origin")  

targetXAxisDesc :: Sentence
targetXAxisDesc = S "The" +:+ phrase target +:+ S "lies on the" +:+ phrase xAxis +:+. sParen (S "from" +:+ makeRef2S neglectCurv)

posXDirectionDesc :: Sentence
posXDirectionDesc = S "The positive" +:+ phrase xDir `sIs` S "from the" +:+. (phrase launcher `toThe` phrase target)

constAccelDesc :: Sentence
constAccelDesc = S "The" +:+ (phrase acceleration `sIs` S "constant") +:+.
                 sParen (S "from" +:+ foldlList Comma List (map makeRef2S [accelXZero, accelYGravity, neglectDrag, freeFlight]))

accelXZeroDesc :: Sentence
accelXZeroDesc = S "The" +:+ phrase acceleration +:+. (S "in the" +:+ phrase xDir `sIs` S "zero")

accelYGravityDesc :: Sentence
accelYGravityDesc = S "The" +:+ phrase acceleration +:+ S "in the" +:+ phrase yDir `isThe` phrase acceleration +:+
                    S "due to" +:+ phrase gravity +:+. sParen (S "from" +:+ makeRef2S yAxisGravity)

neglectDragDesc :: Sentence
neglectDragDesc = S "Air drag" `sIs` S "neglected."

pointMassDesc :: Sentence
pointMassDesc = (S "size" `sAnd` S "shape") `ofThe'` phrase projectile `sAre`
                S "negligible" `sC` S "so that it can be modelled as a point" +:+. phrase mass

freeFlightDesc :: Sentence
freeFlightDesc = S "The flight" `sIs` S "free; there" `sAre` S "no" +:+ plural collision +:+
                 S "during" +:+. (S "trajectory" `ofThe` phrase projectile)

neglectCurvDesc :: Sentence
neglectCurvDesc = S "The" +:+ phrase distance `sIs` S "small enough that" +:+.
                  (S "curvature" `ofThe` S "Earth can be neglected")

timeStartZeroDesc :: Sentence
timeStartZeroDesc = atStart time +:+. S "starts at zero"

gravAccelValueDesc :: Sentence
gravAccelValueDesc = S "The" +:+ phrase acceleration +:+ S "due to" +:+
  phrase gravity +:+ S "is assumed to have the" +:+ phrase value +:+ 
  S "provided in" +:+. makeRef2S (SRS.valsOfAuxCons ([]::[Contents]) 
  ([]::[Section]))
  
  
  
-- alphabetical: all-productive, always-decay, const-conditions, const-growth, const-volume, initial-inf, neglect-drugs, neglect-sick, proportional
  
  
  
assumptions :: [ConceptInstance]
assumptions = [initial-inf, const-growth, const-volume, const-conditions, all-productive, 
  always-decay, neglect-drugs, neglect-sick, clear-proportional]


initial-inf, const-growth, const-volume, const-conditions, all-productive,
  always-decay, neglect-drugs, neglect-sick, 
  clear-proportional :: ConceptInstance
initial-inf         = cic "initial-inf"        initial-infDesc         "initial-inf"        assumpDom
const-growth        = cic "const-growth"       const-growthDesc        "const-growth"       assumpDom
const-volume        = cic "const-volume"       const-volumeDesc        "const-volume"       assumpDom
const-conditions    = cic "const-conditions"   const-conditionsDesc    "const-conditions"   assumpDom
all-productive      = cic "all-productive"     all-productiveDesc      "all-productive"     assumpDom
always-decay        = cic "always-decay"       always-decayDesc        "always-decay"       assumpDom
neglect-drugs       = cic "neglect-drugs"      neglect-drugsDesc       "neglect-drugs"      assumpDom
neglect-sick        = cic "neglect-sick"       neglect-sickDesc        "neglect-sick"       assumpDom
proportional        = cic "proportional"       proportionalDesc        "proportional"       assumpDom

initial-inf :: Sentence
initial-infDesc = S "Initial infection of an HIV patient assumed."

const-growth :: Sentence
const-growthDesc = S "The virions will invade uninfected cells at a constant rate."

const-volume :: Sentence
const-volumeDesc = S "The dimensions of the location associated with the infection remains constant."

const-conditions :: Sentence
const-conditionsDesc = S "Temperature of the location associated with the infection remains constant."

all-productive :: Sentence
all-productiveDesc = S "All infected cells are infect other cells productively."

always-decay :: Sentence
always-decayDesc = S "After viremia peak, no significant upward trends occur."

neglect-drugs :: Sentence
neglect-drugsDesc = S "The effect of drugs on the elimination rate will be not be considered."

neglect-sick :: Sentence
neglect-sickDesc = S "The effect of other infections on the elimination rate will be not be considered."

proportional :: Sentence
proportionalDesc = S "The elimination of the virus is assumed to be proportional to the amount of viruses present."




