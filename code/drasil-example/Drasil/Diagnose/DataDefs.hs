module Drasil.Diagnose.DataDefs (dataDefs) where

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

  
----- DATA DEFINITION

dataDefs :: [DataDefinition]
dataDefs = [viralLoadDD, rateElimDD]

----- Viral Load

viralLoadDD :: DataDefinition
viralLoadDD = ddNoRefs viralLoadQD Nothing "viralLoad" [viralLoadDesc] 

viralLoadQD :: QDefinition
viralLoadQD = mkQuantDef vLoadt viralLoadEqn

viralLoadEqn :: Expr
viralLoadEqn = sy numberV / sy vol

viralLoadDesc :: Sentence
viralLoadDesc = foldlSent [S "The viral load describes the concentration of ", 
  S "a virus within the body at a certain time"]

---

rateElimDD :: DataDefinition
rateElimDD = ddNoRefs rateElimQD Nothing "rateElim" [rateElimDesc]

rateElimQD :: QDefinition
rateElimQD = mkQuantDef vRate rateElimEqn

rateElimEqn :: Expr
rateElimEqn = sy numberV / (sy vol * sy time) 

rateElimDesc :: Sentence
rateElimDesc = foldlSent [S "The viral load describes the concentration of ", 
  S "a virus within the body at a certain time"]
  
  



  


