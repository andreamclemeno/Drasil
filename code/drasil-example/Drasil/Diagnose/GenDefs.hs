module Drasil.Diagnose.GenDefs (genDefns) where
--module Drasil.Diagnose.GenDefs (genDefns, rateElimGD) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, TheoryModel, gd, gdNoRefs)
import Utils.Drasil

---------------- temp

import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (oneD, rectilinear, twoD)

import Data.Drasil.Quantities.Physics (acceleration, constAccelV, iPos, iSpeed,
  iVel, ixPos, ixVel, iyPos, iyVel, position, scalarAccel, scalarPos, speed,
  time, velocity, xAccel, xConstAccel, xPos, xVel, yAccel, yConstAccel, yPos, yVel)
import qualified Data.Drasil.Quantities.Physics as QP (constAccel)

---

import Drasil.Diagnose.Assumptions 
import Drasil.Diagnose.References (hibbeler2004) -- ref?
import Drasil.Diagnose.TMods

genDefns :: [GenDefn]
genDefns = []
--genDefns = [rateElimGD]

----------
--rateElimGD :: GenDefn
--rateElimGD = gd rateElimRC (getUnit speed) (Just rateElimDeriv)
--  [makeCiteInfo hibbeler2004 $ Page [8]] "rateElim" [{-Notes-}]
  
--rateElimGD = gd rateElimRC (units) (derivation)
--  [reference] "rateElim" [{-Notes-}]

--rateElimRC :: RelationConcept
--rateElimRC = makeRC "rateElimRC" (nounPhraseSent $ foldlSent_ 
--            [ S "Rate of ELimination "])
--            EmptyS rateElimRel

--rateElimRel :: Relation -- 
--rateElimRel = sy speed $= sy iSpeed + sy QP.constAccel * sy time

--rateElimDeriv :: Derivation
--rateElimDeriv = mkDerivName (phrase rectilinear +:+ phrase velocity)
--               (weave [rateElimDerivSents, map E rateElimDerivEqns])

--rateElimDerivSents :: [Sentence]
--rateElimDerivSents = [rectDeriv velocity acceleration motSent iVel accelerationTM, rearrAndIntSent, performIntSent]
--  where
--    motSent = foldlSent [S "The motion" `sIn` makeRef2S accelerationTM `sIs` S "now", phrase oneD,
--                         S "with a", phrase QP.constAccel `sC` S "represented by", E (sy QP.constAccel)]

--rateElimDerivEqns :: [Expr]
--rateElimDerivEqns = [rateElimDerivEqn1, rateElimDerivEqn2, rateElimRel]

--rateElimDerivEqn1, rateElimDerivEqn2 :: Expr
--rateElimDerivEqn1 = sy QP.constAccel $= deriv (sy speed) time
--rateElimDerivEqn2 = defint (eqSymb speed) (sy iSpeed) (sy speed) 1 $=
--                   defint (eqSymb time) 0 (sy time) (sy QP.constAccel)
