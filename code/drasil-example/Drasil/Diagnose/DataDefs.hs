module Drasil.Diagnose.DataDefs (dataDefs, viralLoadDD) where

import Language.Drasil
import Utils.Drasil
import Theory.Drasil (DataDefinition, GenDefn, TheoryModel, ddNoRefs, gd, 
  mkQuantDef, tmNoRefs)
import Data.Drasil.Concepts.Documentation (body, component, constant, material_,
  value)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Quantities.Physics (speed, iSpeed, ixVel, iyVel, velocity)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (vol, amt)

import Drasil.Diagnose.Unitals 
import Drasil.Diagnose.Assumptions

  
----- DATA DEFINITION

dataDefs :: [DataDefinition]
dataDefs = [viralLoadDD]

----- Viral Load

viralLoadDD :: DataDefinition
viralLoadDD = ddNoRefs viralLoadQD Nothing "viralLoad" [viralLoadDesc] 

viralLoadQD :: QDefinition
viralLoadQD = mkQuantDef vLoad viralLoadEqn

viralLoadEqn :: Expr
viralLoadEqn = sy numberV / sy vol

viralLoadDesc :: Sentence
viralLoadDesc = foldlSent [S "The viral load describes the concentration of ", 
  S "a virus within the body at a certain time. It assumes that the volume of blood is constant with respect to", makeRef2S constVolume]

  


