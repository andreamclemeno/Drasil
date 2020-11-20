module Drasil.Diagnose.DataDefs (dataDefs,viralLoadDD) where

import Language.Drasil
import Utils.Drasil
import Theory.Drasil (DataDefinition, GenDefn, TheoryModel, ddNoRefs, gd, 
  mkQuantDef, tmNoRefs)
import Data.Drasil.Concepts.Documentation (body, component, constant, material_,
  value)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Quantities.Physics (speed, iSpeed, ixVel, iyVel, velocity)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (vol, amt)

  
----- DATA DEFINITION

dataDefs :: [DataDefinition]
dataDefs = [viralLoadDD]

----- Viral Load

viralLoadDD :: DataDefinition
viralLoadDD = ddNoRefs viralLoadQD Nothing "viralLoad" [viralLoadDesc] 

viralLoadQD :: QDefinition
viralLoadQD = mkQuantDef speed viralLoadEqn

viralLoadEqn :: Expr
viralLoadEqn = (sy QPP.amt)/(sy QPP.vol)

viralLoadDesc :: Sentence
viralLoadDesc = foldlSent [S "The viral load describes the concentration of ", 
  S "a virus within the body at a certain time"]

