module Drasil.NoPCM.Body where

import Control.Lens ((^.))
import Prelude hiding (id)
import Drasil.NoPCM.Example

import Language.Drasil

import Data.Drasil.SI_Units 
import Data.Drasil.Authors
import Data.Drasil.Utils(listConstS)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (ode, equation)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Concepts.Thermodynamics (heat)
import Data.Drasil.Units.Thermodynamics
import Data.Drasil.Quantities.Thermodynamics (temp, ht_flux)

import Drasil.ReferenceMaterial (intro)
import qualified Drasil.SRS as SRS
import Drasil.DocumentLanguage
import Drasil.OrganizationOfSRS

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s2, s2_3, s3, s3_1, s4, s4_1, s4_1_1, s4_1_2, s4_1_3, s4_2, s5, s5_2, s6 :: Section

s3_1_intro, sys_context_fig, s4_1_intro, s4_1_1_bullets, s4_1_2_list, s4_1_3_intro,
  s4_1_3_list, fig_tank, s4_2_3_intro, s4_2_4_intro, s4_2_5_intro, s4_2_6_table1, s4_2_6_table2:: Contents

-------------------------------
--Section 1 : REFERENCE MATERIAL
-------------------------------
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit (nw ht_trans), Doc' (nw sWHS)], SymbOrder], TAandA]) : 
        map Verbatim [s2, s3, s4, s5, s6]  
        
pcm_si :: SystemInformation
pcm_si = SI srs_swhs srs [thulasi] this_si pcmSymbols (pcmSymbols) acronyms
  
pcm_srs :: Document
pcm_srs = mkDoc mkSRS pcm_si

nopcmSymbMap :: SymbolMap
nopcmSymbMap = symbolMap pcmSymbols


--------------------------
--Section 2 : INTRODUCTION
--------------------------

s2 = SRS.intro [] [s2_3]

s2_3 = charIntRdrF knowledge understanding (getAcc sWHS) (SRS.userChar SRS.missingP []) --FIXME: referencing this for now until we figure out how to reference auto-generated section (section 3.2)
  where knowledge = ((phrase $ heat ^. term) +:+ S "transfer" +:+. (phrase $ theory ^. term) +:+
                    S "A third or fourth year Mechanical Engineering course on this topic is recommended")
        understanding = (S "differential" +:+ (plural $ equation ^. term) `sC` S "as typically" +:+
                        S "covered in first and second year Calculus courses")

                        
----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

s3 = genSysF [s3_1] (Paragraph $ EmptyS) Nothing []
--TODO: fill in the empty (last three) parameters

s3_1 = SRS.sysCont [s3_1_intro, sys_context_fig] []

s3_1_intro = Paragraph $
              (makeRef sys_context_fig) +:+ S "shows the" +:+. (phrase $ sysCont ^. term) +:+
             S "A circle represents an external entity outside the" +:+ (phrase $ software ^. term) `sC`
             S "the" +:+ (phrase $ user ^. term) +:+ S "in this case. A rectangle represents the" +:+ (phrase $ softwareSys ^. term) +:+
             S "itself" +:+. sParen (getAcc sWHS) +:+ S "Arrows are used to show the" +:+ (plural $ datum ^. term) +:+ S "flow between the" +:+
              (phrase $ section_ ^. term) +:+ S "and its" +:+. (phrase $ environment ^. term)
            
sys_context_fig = Figure ((makeRef sys_context_fig) :+: S ":" +:+ (titleize $ sysCont ^. term))
            "SystemContextFigure.png"


-----------------------------------------
--Section 4 : SPECIFIC SYSTEM DESCRIPTION
-----------------------------------------

--TODO: finish filling in the subsections
s4 = specSysDesF (words_) [s4_1, s4_2]
  where words_ = (plural definition +:+ S "and finally the" +:+
                (phrase $ inModel ^. term) +:+ sParen (getAcc ode) +:+
                S "that models the" +:+ (phrase $ sWHT ^. term))

s4_1 = SRS.probDesc [s4_1_intro] [s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = Paragraph $
            (getAcc sWHS) +:+ S "is a computer" +:+ (phrase $ program ^. term) +:+ S "developed to investigate" +:+
           S "the heating of" +:+ (phrase $ water ^. term) +:+ S "in a" +:+. (phrase $ sWHT ^. term)

s4_1_1 = termDefnF Nothing [s4_1_1_bullets]
  
s4_1_1_bullets = Enumeration $ (Bullet $ map (\c -> Flat $ 
          ((at_start $ c ^. term)) :+: S ":" +:+ (c ^. defn)) 
          [thermal_flux, heat_cap_spec])
  
s4_1_2 = physSystDesc (getAcc sWHS) fig_tank [s4_1_2_list, fig_tank]

fig_tank = Figure ((at_start $ sWHT ^. term) `sC` S "with" +:+ (phrase $ ht_flux ^. term) +:+ S "from" +:+ (phrase $ coil ^. term) +:+ S "of" +:+
            P (ht_flux_C ^. symbol)) "TankWaterOnly.png"
  
s4_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
            (S "PS1", (at_start $ tank ^. term) +:+ S "containing" +:+ (phrase $ water ^. term)), 
            (S "PS2", S "Heating" +:+ (phrase $ coil ^. term) +:+ S "at bottom of" +:+. (phrase $ tank ^. term) +:+
           sParen (P (ht_flux_C ^. symbol) +:+ S "represents the" +:+ (phrase $ ht_flux_C ^. term) +:+
           S "into the" +:+. (phrase $ water ^. term)))]

s4_1_3 = SRS.goalStmt [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro = Paragraph $
           S "Given the" +:+ (phrase $ temp ^. term) +:+ S "of the" +:+ (phrase $ coil ^. term) `sC` S "initial" +:+
            (phrase $ temp ^. term) +:+ S "of the" +:+ (phrase $ water ^. term) `sC`
           S "and material" +:+ (plural $ property ^. term) `sC` S "the goal statement is"

s4_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
            (S "GS1", S "predict the" +:+ (phrase $ temp_water ^. term) +:+ S "over time")]

s4_2 = solChSpecF sWHS (s4_1, s6) True EmptyS (((makeRef s4_2_6_table1) +:+ S "and" +:+ (makeRef s4_2_6_table2) +:+ S "show"), EmptyS, False, EmptyS)
          ([], s4_2_2_TMods, [s4_2_3_intro], [s4_2_4_intro], [s4_2_5_intro], [s4_2_6_table1, s4_2_6_table2]) []
  
s4_2_2_TMods :: [Contents]
s4_2_2_TMods = map (Definition nopcmSymbMap . Theory) [t1consThermE]

s4_2_3_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_4_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_5_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, S "Typical Value"]
  (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map (listConstS) []) 
    (S "Table 1: Input Variables") True

s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint, S "Typical Value"]
  (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map (listConstS) []) 
    (S "Table 2: Output Variables") True
    

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

s5 = reqF [s5_2] --TODO: Add the rest of the section

s5_2 = nonFuncReqF [S "performance"] [S "correctness", S "verifiability",
        S "understandability", S "reusability", S "maintainability"]
        (S "This problem is small in size and relatively simple")
        (S "Any reasonable implementation will be very quick and use minimal storage.")

----------------------------
--Section 6 : LIKELY CHANGES
----------------------------

s6 = SRS.likeChg [] [] --TODO: Add the rest of the section