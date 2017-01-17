module Drasil.PCM.Example where

import Language.Drasil

import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as U
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP
import Prelude hiding (id)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Math
import Data.Drasil.Concepts.Math (ode) 
import qualified Data.Drasil.Quantities.Physics as QP (time)

import Control.Lens ((^.))

pcmSymbols :: [UnitalChunk]
pcmSymbols = [coil_SA,hIn_SA,hOut_SA,htCap,htCap_Liq,htCap_W,tank_D,ht_gen_vol,
  ht_xfer_co,ht_xfer_CW,tank_L,mass,water_m, -- norm_vect, 
  ht_flux, thFluxVect,
  ht_flux_C,ht_flux_in,ht_flux_out,time,temp, --temp_boil,
  temp_coil,temp_env,time_final,temp_init,temp_water,temp_diff,vol, --tank_vol,
  water_vol,density,water_dense,dummyVar]

coil_SA, hIn_SA, hOut_SA, htCap, htCap_Liq, htCap_W, tank_D, ht_gen_vol,
  ht_xfer_co,ht_xfer_CW, tank_L,mass,water_m,ht_flux,thFluxVect,
  ht_flux_C,ht_flux_in,ht_flux_out,time,temp,--temp_boil,
  temp_coil,temp_env,time_final,temp_init,temp_water,temp_diff,vol,--tank_vol,
  water_vol,density,water_dense,dummyVar :: UnitalChunk

coil_SA     = makeUC "A_C" "coil surface area" 
  "FIXME: Define this or remove the need for definitions" (sub cA cC) m_2
hIn_SA      = makeUC "A_in" "surface area over which heat is transferred in" 
  "FIXME: Define this or remove the need for definitions" (sub cA (Atomic "in")) m_2
hOut_SA     = makeUC "A_out" "surface area over which heat is transferred out" 
  "FIXME: Define this or remove the need for definitions" (sub cA (Atomic "out")) m_2
htCap       = ucFromVC heat_cap_spec U.heat_cap_spec
htCap_Liq   = makeUC "C^L" "specific heat capacity of a liquid" 
  "FIXME: Define this or remove the need for definitions" (sup cC cL) U.heat_cap_spec
htCap_W     = makeUC "C_W" "specific heat capacity of water" 
  "FIXME: Define this or remove the need for definitions" (sub cC cW) U.heat_cap_spec
tank_D      = makeUC "D" "diameter of tank" 
  "FIXME: Define this or remove the need for definitions" cD metre
ht_gen_vol  = makeUC "g" "volumetric heat generation per unit volume" 
  "FIXME: Define this or remove the need for definitions" lG U.thermal_flux
ht_xfer_co  = makeUC "h" "convective heat transfer coefficient" 
  "FIXME: Define this or remove the need for definitions" lH U.heat_transfer_coef
ht_xfer_CW  = makeUC "h_C" "convective heat transfer between coil and water"   "FIXME: Define this or remove the need for definitions" 
  (sub lH cC) U.heat_transfer_coef
tank_L      = makeUC "L" "length of tank" 
  "FIXME: Define this or remove the need for definitions" cL metre
mass        = ucFromVC QPP.mass kilogram
water_m     = makeUC "m_W" ((QPP.mass ^. id) ++ " of water")
  "FIXME: Define this or remove the need for definitions" 
  (sub (QPP.mass ^. symbol) cW) kilogram
  -- How do I make a symbol that needs one (or more) Accent? Add to Symbol or
  -- pull Accent out somehow?
ht_flux     = makeUC "q" "heat flux" 
  "FIXME: Define this or remove the need for definitions" lQ U.heat_transfer_coef
thFluxVect  = makeUC "q_vect" "thermal flux vector" 
  "FIXME: Define this or remove the need for definitions" (vec lQ) 
  U.thermal_flux
ht_flux_C   = makeUC "q_C" "heat flux from coil" 
  "FIXME: Define this or remove the need for definitions" (sub lQ cC) 
  U.thermal_flux
ht_flux_in  = makeUC "q_in" "heat flux in" 
  "FIXME: Define this or remove the need for definitions" (sub lQ (Atomic "in")) 
  U.thermal_flux
ht_flux_out = makeUC "q_out" "heat flux out" 
  "FIXME: Define this or remove the need for definitions" (sub lQ (Atomic "out")) U.thermal_flux
time        = ucFromVC QP.time second
temp        = makeUC "T" "temperature" 
  "FIXME: Define this or remove the need for definitions" cT centigrade
-- temp_boil   = makeUC "T_boil" "temperature at boiling point" -- (sub cT (Atomic "boil")) centigrade
temp_coil   = makeUC "T_coil" "temperature of coil" 
  "FIXME: Define this or remove the need for definitions" (sub cT cC) centigrade
temp_env    = makeUC "T_env" "temperature of environment" 
  "FIXME: Define this or remove the need for definitions" (sub cT (Atomic "env")) 
  centigrade
time_final  = makeUC "t_final" "time" 
  "FIXME: Define this or remove the need for definitions" (sub lT (Atomic "final")) second
temp_init   = makeUC "T_init" "initial temperature" 
  "FIXME: Define this or remove the need for definitions" (sub cT (Atomic "init")) 
  centigrade
temp_water  = makeUC "T_W" "temperature of water" 
  "FIXME: Define this or remove the need for definitions" (sub cT cW) centigrade
temp_diff   = makeUC "deltaT" "temperature difference" 
  "FIXME: Define this or remove the need for definitions" (Concat [Greek Delta, cT]) 
  centigrade
vol         = makeUC "V" "volume" 
  "FIXME: Define this or remove the need for definitions" cV m_3
--tank_vol    = makeUC "V_tank" "volume of the cylindrical tank"   -- (sub cV (Atomic "tank")) m_3
water_vol   = makeUC "V_W" "volume of water" 
  "FIXME: Define this or remove the need for definitions" (sub cV cW) m_3
density     = makeUC "rho" "density, mass per unit volume" 
  "FIXME: Define this or remove the need for definitions" (Greek Rho_L) densityU
water_dense = makeUC "rho_W" "density of water" 
  "FIXME: Define this or remove the need for definitions" (sub (Greek Rho_L) cW)
  densityU
dummyVar    = makeUC "tau" "dummy variable for integration over time"
  "FIXME: Define this or remove the need for definitions" (Greek Tau_L) second
--melt_frac   = makeUC "Phi" "melt fraction" (Greek Phi) unitless

----Acronyms-----
acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ode,
  physSyst,requirement,srs,sWHS,thModel]
  
sWHS :: ConceptChunk
sWHS = dcc "sWHS" "SWHS" "Solar Water Heating System"

----EqChunks----
--Theoretical models--
t1consThermE :: RelationConcept
t1consThermE = makeRC "t1consThermE" "Conservation of thermal energy" 
  t1descr cons_therm_rel

cons_therm_rel :: Relation
cons_therm_rel = (Neg (C gradient)) :. (C thFluxVect) + (C ht_gen_vol) := 
  (C density) * (C htCap) * (Deriv Part (C temp) (C time))

t1descr :: Sentence
t1descr = 
  (S ("This equation gives the conservation of energy for time varying heat " ++
  "transfer in a material of specific heat capacity ") :+: 
  (P $ htCap ^. symbol) :+: S " and density " :+: (P $ density ^. symbol) :+:
  S ", where " :+: (P $ thFluxVect ^. symbol)) 
  --TODO: Finish this description and do it better. I need to
  --  figure out the best way to encode this information.