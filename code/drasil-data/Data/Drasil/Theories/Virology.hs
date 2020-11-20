module Data.Drasil.Theories.Virology where

import Language.Drasil
import Theory.Drasil (DataDefinition, GenDefn, TheoryModel, ddNoRefs, gd, 
  mkQuantDef, tmNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (body, component, constant, material_,
  value)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (gravity, twoD)
import qualified Data.Drasil.Quantities.Math as QM (unitVectj)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (vol, amt)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, 
  force, gravitationalAccel, height, pressure, torque, weight, positionVec)
  
import Data.Drasil.SI_Units (mole, litre)
  
----- THEORETICAL MODEL

----- (1) Determine if Post Viremia

----- (2) Exponential Viral Elimination



----- GENERAL DEFINITION

----- Rate of Elimination



----- DATA DEFINITION

----- Viral Load

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (vol, amt)

viralLoadDD :: DataDefinition
viralLoadDD = ddNoRefs viralLoad Nothing "viralLoad" [viralLoadDesc] 

viralLoad :: QDefinition
viralLoad = mkQuantDef QP.viralLoad viralLoadEqn

viralLoadEqn :: Expr
viralLoadEqn = (sy QPP.amt)/(sy QPP.vol)

viralLoadDesc :: Sentence
viralLoadDesc = foldlSent [S "The viral load describes the concentration of ", 
  S "a virus within the body at a certain time"]




