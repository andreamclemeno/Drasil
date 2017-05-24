module Drasil.GamePhysics.Body where

import Drasil.Template.MG 
import Drasil.Template.DD
import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.SI_Units


import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Physics (rigidBody, elasticity, cartesian, friction, 
                   rightHand, collision, space)
import Data.Drasil.Concepts.PhysicalProperties (ctrOfMass)
import Data.Drasil.Concepts.Math
import Data.Drasil.Utils (foldle, foldlSent, mkEnumAbbrevList, mkConstraintList, 
  makeTMatrix)
import Data.Drasil.Software.Products
import Data.Drasil.Quantities.Physics (restitutionCoef, time)
import Data.Drasil.Quantities.PhysicalProperties (mass, len)

import Drasil.SpecificSystemDescription
import Drasil.OrganizationOfSRS
import qualified Drasil.SRS as SRS
import qualified Drasil.ReferenceMaterial as RM

import Drasil.GamePhysics.Unitals
import Drasil.GamePhysics.Concepts
import Drasil.GamePhysics.TMods
import Drasil.GamePhysics.IMods
import Drasil.GamePhysics.DataDefs

import Drasil.GamePhysics.Modules
import Drasil.GamePhysics.Changes
import Drasil.GamePhysics.Reqs

import Drasil.DocumentLanguage

authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = manyNames authors

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc' mkSRS for' chipmunkSysInfo

mkSRS :: DocDesc
mkSRS = RefSec (RefProg RM.intro [TUnits, tsymb tableOfSymbols, TAandA ]) : 
  map Verbatim [s2, s3, s4, s5, s6, s7, s8]
  where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder]

    --FIXME: Need to be able to print defn for gravitational constant.

chipmunkSysInfo :: SystemInformation
chipmunkSysInfo = SI chipmunk srs authors chipUnits cpSymbols ([] :: [CQSWrapper])
  cpAcronyms --FIXME: All named ideas, not just acronyms.

chipUnits :: [UnitDefn]
chipUnits = map UU [metre, kilogram, second] ++ map UU [newton, radians]

chipmunkMG :: Document
chipmunkMG = mgDoc' chipmunk auths mgBod

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

--FIXME: The SRS has been partly switched over to the new docLang, so some of
-- the sections below are now redundant. I have not removed them yet, because
-- it makes it easier to test between the two different versions as changes
-- are introduced. Once the SRS is entirely switched to docLang, the redundant
-- sections should be removed.

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section : INTRODUCTION --
------------------------------

s2 :: Section
s2_intro :: [Contents]

s2 = SRS.intro (s2_intro) [s2_1, s2_2, s2_3]

para1_s2_intro :: Contents
para1_s2_intro = Paragraph $ foldlSent
  [S "Due to the rising cost of developing", (plural videoGame) `sC` 
  S "developers are looking for ways to save time and money for their" +:+.
  (plural project), S "Using an", (phrase openSource), 
  (phrase $ physLib ^. term),
  S "that is reliable and free will cut down development costs and lead",
  S "to better quality", (plural product_)]

para2_s2_intro :: Contents
para2_s2_intro = Paragraph $ foldlSent 
  [S "The following", (phrase section_), S "provides an overview of the",
  titleize srs, (sParen $ getAcc srs), S "for",
  (short chipmunk) `sC` S "an", (phrase openSource), (getAcc twoD), 
  (phrase $ rigidBody ^. term) +:+. (phrase $ physLib ^. term),
  S "This", (phrase section_), S "explains the", (phrase purpose), S "of this", 
  (phrase document) :+: S ", the scope", S "of the", (phrase system) `sC` 
  S "and the", (phrase organization), S "of the", (phrase document)]
        
s2_intro = [para1_s2_intro, para2_s2_intro]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

s2_1 :: Section
s2_1_intro :: [Contents]

s2_1 = SRS.prpsOfDoc (s2_1_intro) []

para1_s2_1_intro :: Contents
para1_s2_1_intro = Paragraph $ foldlSent 
  [S "This", (phrase document), S "descibes the modeling of an",
  (phrase openSource), getAcc twoD, (phrase $ rigidBody ^. term), 
  (phrase $ physLib ^. term), S "used for" +:+. (plural game), S "The", 
  plural goalStmt, S "and", plural thModel, S "used in",
  short chipmunk, S "are provided. This", (phrase document),
  S "is intended to be used as a reference to provide all",
  S "necessary", (phrase information), S "to understand and verify the", 
  (phrase model)]

para2_s2_1_intro :: Contents
para2_s2_1_intro = Paragraph $ foldlSent 
  [S "This", (phrase document), S "will be used as a starting point for",
  S "subsequent development phases, including writing the design",
  S "specification and the", (phrase software), (phrase vav), S "plan.",
  S "The design", (phrase document), S "will show how the", plural requirement, 
  S "are to be realized.", 
  S "The", (phrase vav), S "plan will show the steps",
  S "that will be used to increase confidence in the", (phrase softwareDoc),
  S "and the implementation"]

s2_1_intro = [para1_s2_1_intro, para2_s2_1_intro]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

s2_2 :: Section
s2_2_intro :: Contents

s2_2 = SRS.scpOfReq [s2_2_intro] []

s2_2_intro = Paragraph $ foldlSent 
  [S "The scope of the", plural requirement, S "includes the",
  (phrase $ physicalSim),  S "of", (getAcc twoD), (plural $ rigidBody ^. term),
  S "acted on by forces. Given", (getAcc twoD), 
  (plural $ rigidBody ^. term) `sC` (short chipmunk), 
  S "is intended to simulate how these", (plural $ rigidBody ^. term), 
  S "interact with one another"]

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

s2_3 :: Section
s2_3_intro :: Sentence

s2_3 = orgSec s2_3_intro inModel s4_2_5

-- FIXME: Citations.
-- FIXME: This can probably be completely pulled out is we decide on the 
--  right convention for the intro across examples.
s2_3_intro = foldlSent 
  [S "The", (phrase organization), S "of this", (phrase document), S "follows the",
  S "template for an", (getAcc srs), S "for scientific",
  S "computing", (phrase software), S "proposed by [1] and [2]"]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

s3 :: Section
s3_intro :: Contents

s3 = SRS.genSysDec [s3_intro] [s3_1, s3_2]

--FIXME: This can be generalized to use more chunks
s3_intro = Paragraph $ foldlSent 
  [S "This", (phrase section_), S "provides", (phrase general), (phrase information),
  S "about the", (phrase system) `sC` S "identifies the interfaces between the", 
  (phrase system), S "and",
  S "its environment, and describes the", (plural userCharacteristic), 
  S "and the", (plural systemConstraint)]

--------------------------------
-- 3.1 : User Characteristics --
--------------------------------

s3_1 :: Section
s3_1_intro :: Contents

s3_1 = SRS.userChar [s3_1_intro] []

s3_1_intro = Paragraph $ foldlSent 
  [S "The end user of", (short chipmunk),
  S "should have an understanding of first year programming concepts",
  S "and an understanding of high school", (phrase physics)]

-------------------------------
-- 3.2 : System Constraints  --
-------------------------------

s3_2 :: Section
s3_2_intro :: Contents

s3_2 = SRS.sysCon [s3_2_intro] []

s3_2_intro = Paragraph $ S "There are no" +:+. (plural systemConstraint)

---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

s4 :: Section

s4 = specSysDescr physLib [s4_1, s4_2]

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1 :: Section
s4_1_intro :: Contents

s4_1 = SRS.probDesc [s4_1_intro] [s4_1_1, s4_1_2]

s4_1_intro = Paragraph $ foldlSent 
  [S "Creating a gaming", (phrase $ physLib ^. term),
  S "is a difficult task.", (titleize' game), S "need", 
  (plural $ physLib ^. term), S "that simulate", 
  S "objects acting under various", (phrase physical), S "conditions, while", 
  S "simultaneously being fast and efficient enough to work in soft",
  (phrase realtime), S "during the" +:+. (phrase game), S "Developing a", 
  (phrase $ physLib ^. term),
  S "from scratch takes a long period of time and is very costly" `sC`
  S "presenting barriers of entry which make it difficult for", (phrase game),
  S "developers to include", (phrase physics), 
  S "in their" +:+. (plural product_), S "There are a few", S "free,", (phrase openSource), 
  S "and high quality", (plural $ physLib ^. term), S "available to",
  S "be used for consumer", (plural product_) +:+. (sParen $ makeRef s7),
  S "By creating a simple, lightweight, fast and portable",
  (getAcc twoD), (phrase $ rigidBody ^. term), (phrase $ physLib ^. term) `sC`
  (phrase game), S "development will be more accessible",
  S "to the masses and higher quality", (plural product_), S "will be produced"]

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1 :: Section
s4_1_1_intro, s4_1_1_bullets :: Contents

s4_1_1 = SRS.termAndDefn [s4_1_1_intro, s4_1_1_bullets] []

s4_1_1_intro = Paragraph $ foldle (+:+) (+:) (EmptyS) 
  [S "This subsection provides a list of terms",
  S "that are used in subsequent", (plural section_), 
  S "and their meaning, with the", (phrase purpose), 
  S "of reducing ambiguity and making it easier to correctly",
  S "understand the", plural requirement]


s4_1_1_bullets = Enumeration (Bullet $ 
  (map (\x -> Flat $ (at_start $ x ^. term) :+: S ":" +:+ (x ^. defn)))
  [rigidBody, elasticity, ctrOfMass, cartesian, rightHand])


-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

s4_1_2 :: Section
s4_1_2_list :: Contents

s4_1_2 = SRS.goalStmt [s4_1_2_list] []

s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, s4_1_2_stmt4 :: Sentence
s4_1_2_stmt1 = foldlSent 
  [S "Given the", (plural physicalProperty) `sC` S "initial", 
  (plural $ position ^. term), S "and",
  (plural $ vel ^. term) `sC` S "and", (plural $ force ^. term),
  S "applied on a set of", (plural $ rigidBody ^. term) `sC`
  S "determine their new", (plural $ position ^. term), S "and",
  (plural $ vel ^. term), S "over a period of", (phrase $ time ^. term)]

s4_1_2_stmt2 = foldlSent 
  [S "Given the", (plural physicalProperty) `sC` S "initial", 
  (plural $ orientation ^. term), S "and", (plural $ angVel ^. term) `sC`
  S "and", (plural $ force ^. term), S "applied on a set of", 
  (plural $ rigidBody ^. term) `sC` S "determine their new",
  (plural $ orientation ^. term), S "and", (plural $ angVel ^. term), 
  S "over a period of", (phrase $ time ^. term)]

s4_1_2_stmt3 = foldlSent 
  [S "Given the initial", (plural $ position ^. term), S "and", 
  (plural $ vel ^. term), S "of a", S "set of", 
  (plural $ rigidBody ^. term) `sC` S "determine if any of",
  S "them will collide with one another over a period of", 
  (phrase $ time ^. term)]

s4_1_2_stmt4 = foldlSent 
  [S "Given the", (plural physicalProperty) :+: S ",", S "initial linear and angular", 
  (plural $ position ^. term), 
  S "and", (plural $ vel ^. term) `sC` S "determine the new",
  (plural $ position ^. term), S "and", (plural $ vel ^. term),
  S "over a period of", (phrase $ time ^. term), S "of",
  (plural $ rigidBody ^. term), S "that have undergone a", 
  (phrase $ collision ^. term)]

s4_1_2_list' :: [Sentence]
s4_1_2_list' = [s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, s4_1_2_stmt4]

s4_1_2_list = Enumeration (Simple $ mkEnumAbbrevList 1 (getAcc goalStmt) s4_1_2_list')

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

s4_2 :: Section

s4_2 = SRS.solCharSpec []
 [s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5, s4_2_6]

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

s4_2_1 :: Section
s4_2_1_intro, s4_2_1_list :: Contents

s4_2_1 = SRS.assump [s4_2_1_intro, s4_2_1_list] []

-- TODO: Add assumption references in the original and this SRS. --
s4_2_1_intro = Paragraph $ foldlSent 
  [S "This", (phrase section_), S "simplifies the original", (phrase problem),
  S "and helps in developing the", (phrase thModel), S "by filling in the",
  S "missing", (phrase information), S "for the" +:+. (phrase physicalSystem),
  S "The numbers given in", S "the square brackets refer to the", 
  foldr1 sC (map (\ch -> (phrase ch) +:+ (bterm ch)) 
  [thModel, genDefn, dataDefn, inModel]) `sC` S "or", 
  phrase likelyChg, (bterm likelyChg) `sC` S "in which the respective",
  (phrase assumption), S "is used"]
  where bterm chunk = S "[" :+: (getAcc chunk) :+: S "]"

s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4, s4_2_1_assum5, 
  s4_2_1_assum6, s4_2_1_assum7 :: Sentence

s4_2_1_assum1 = foldlSent [S "All objects are", (plural $ rigidBody ^. term)]
s4_2_1_assum2 = foldlSent [S "All objects are", (getAcc twoD)]
s4_2_1_assum3 = foldlSent [S "The library uses a", (phrase $ cartesian ^. term)]
s4_2_1_assum4 = foldlSent [S "The axes are defined using", 
  (phrase $ rightHand ^. term)]
s4_2_1_assum5 = foldlSent [S "All", (plural $ rigidBody ^. term), 
  (plural $ collision ^. term), S "are vertex-to-edge", 
  (plural $ collision ^. term)]
s4_2_1_assum6 = foldlSent [S "There is no damping", 
  S "involved throughout the", (phrase simulation)]
s4_2_1_assum7 = foldlSent [S "There are no", (plural constraint),
  S "and joints involved throughout the", (phrase simulation)]

s4_2_1_list' :: [Sentence]
s4_2_1_list' = [s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4,
               s4_2_1_assum5, s4_2_1_assum6, s4_2_1_assum7]

s4_2_1_list = Enumeration (Simple $ mkEnumAbbrevList 1 (getAcc assumption) s4_2_1_list')


--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

s4_2_2 :: Section
s4_2_2_intro :: Contents
s4_2_2_TMods :: [Contents]

s4_2_2 = SRS.thModel ([s4_2_2_intro] ++ (s4_2_2_TMods)) []

s4_2_2_intro = Paragraph $ foldlSent 
  [S "This", (phrase section_), S "focuses on the", (phrase general), 
  (plural $ equation ^. term), S "the", (phrase $ physLib ^. term), 
  S "is based on"]

s4_2_2_TMods = map Definition (map Theory cpTMods)

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

s4_2_3 :: Section
s4_2_3_intro :: Contents
-- s4_2_3_GDefs :: [Contents]

s4_2_3 = SRS.genDefn ([s4_2_3_intro] {- ++
  (map Con s4_2_3_GDefs)-}) []

s4_2_3_intro = Paragraph $ foldlSent 
  [S "This", (phrase section_), S "collects the laws and", 
  (plural $ equation ^. term), S "that will be used in deriving the", 
  (plural dataDefn) `sC` S "which in turn will be used to build the", 
  (plural inModel)]

-- GDefs not yet implemented --
{-
s4_2_3_GDefs :: [Contents]
s4_2_3_GDefs = map Definition (map General gDefs)
-}

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

s4_2_4 :: Section
s4_2_4_intro :: Contents
s4_2_4_DDefs :: [Contents]

s4_2_4 = SRS.dataDefn ([s4_2_4_intro] ++
  (s4_2_4_DDefs)) []

s4_2_4_intro = Paragraph $ foldlSent [S "This", (phrase section_), 
  S "collects and defines all the", (plural datum), S "needed to build the" +:+. 
  titleize' inModel, S "The dimension of each", (phrase quantity), 
  S "is also given"]

s4_2_4_DDefs = map Definition (map Data cpDDefs)

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5 :: Section
s4_2_5_intro :: Contents
s4_2_5_IMods :: [Contents]

s4_2_5 = SRS.inModel ([s4_2_5_intro] ++
  (s4_2_5_IMods)) []

s4_2_5_intro = Paragraph $ foldlSent 
  [S "This", (phrase section_), S "transforms the", (phrase problem), S "defined",
  S "in", (makeRef s4_1), S "into one expressed in mathematical",
  S "terms. It uses concrete symbols defined in", (makeRef s4_2_4),
  S "to replace the abstract symbols in the", (plural model), S "identified in",
  (makeRef s4_2_2), S "and", (makeRef s4_2_3)]

-- Instance models not fully yet implemented --

s4_2_5_IMods = map Definition (map Theory iModels)

------------------------------
-- Collision Diagram        --
------------------------------
{-- should be paired with the last instance model for this example
secCollisionDiagram = Paragraph $ foldlSent [ S "This section presents an image", 
  S "of a typical collision between two 2D rigid bodies labeled A and B,"  
  S "showing the position of the two objects, the collision normal vector n and",
  S "the vectors from the approximate center of mass of each object to the point",
  S "of collision P, rAP and rBP. Note that this figure only presents", 
  S "vertex-to-edge collisions, as per our assumptions (A5)."]
--}

{--fig_1 = Figure (titleize figure +:+ S "1:" +:+ S "Collision between two rigid bodies")
"CollisionDiagram.png" --}
------------------------------
-- 4.2.6 : Data Constraints --
------------------------------

s4_2_6 :: Section
s4_2_6_intro, s4_2_6_table1, s4_2_6_table2 :: Contents

s4_2_6 = SRS.datCon [s4_2_6_intro, s4_2_6_table1,
  s4_2_6_table2] []

s4_2_6_intro = Paragraph $ foldlSent 
  [(makeRef s4_2_6_table1), S "and", (makeRef s4_2_6_table2), 
  S "show the", (plural datumConstraint), S "on",
  S "the input and output variables, respectively. The",
  (Quote $ titleize' physicalConstraint), S "column gives the", (phrase physical),
  S "limitations on the range of values that can be taken by the",
  S "variable. The", (plural constraint), S "are conservative, to give the user of the",
  (phrase model), S "the flexibility to experiment with unusual situations. The",
  S "column of typical values is intended to provide a feel for a",
  S "common scenario"]

-- Currently unable to write relations in sentences, so verbal explanations
-- will do for now.
-- How do I write 2pi in constraints?



s4_2_6_symbolList :: [(Sentence, Sentence, Sentence, Sentence, Sentence)]
s4_2_6_symbolList = 
  [((P $ len ^. symbol), S "is G/E to 0", EmptyS, S "44.2", (Sy $ unit_symb len)), 
  ((P $ mass ^. symbol), S "is greater than 0", EmptyS, S "56.2", (Sy $ unit_symb mass)), 
  ((P $ momtInert ^. symbol), S "is G/E to 0", EmptyS, S "74.5", (Sy $ unit_symb momtInert)),
  ((P $ gravAccel ^. symbol), EmptyS, EmptyS, S "9.8", (Sy $ unit_symb gravAccel)), 
  ((P $ position ^. symbol), EmptyS, EmptyS, S "(0.412, 0.502)", (Sy $ unit_symb position)), 
  ((P $ vel ^. symbol), EmptyS, EmptyS, S "2.51", (Sy $ unit_symb vel)), 
  ((P $ restitutionCoef ^. symbol), S "G/E to 0", S "less than 1", S "0.8", EmptyS),
  ((P $ orientation ^. symbol), S "G/E to 0", S "less than 2pi", S "pi/2", (Sy $ unit_symb orientation)),  
  ((P $ angVel ^. symbol), EmptyS, EmptyS, S "2.1", (Sy $ unit_symb angVel)),
  ((P $ force ^. symbol), EmptyS, EmptyS, S "98.1", (Sy $ unit_symb force)), 
  ((P $ torque ^. symbol), EmptyS, EmptyS, S "200", (Sy $ unit_symb torque))]

s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, S "Typical Value"]
  (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map (mkConstraintList) s4_2_6_symbolList) 
    (S "Table 1: Input Variables") True

s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint]
  (mkTable [(\x -> x!!0), (\x -> x!!1)] [
  [(P $ position ^. symbol), S "None"],
  [(P $ vel ^. symbol), S "None"],
  [(P $ orientation ^. symbol), (P $ orientation ^. symbol) +:+ S "G/E to 0" +:+
  S "and" +:+ (P $ orientation ^. symbol) +:+ S "less than 2pi"],
  [(P $ angVel ^. symbol), S "None"]
  ]) (S "Table 2: Output Variables") True

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

s5 :: Section
s5_intro :: Contents

s5 = SRS.require [s5_intro] [s5_1, s5_2]

s5_intro = Paragraph $ foldlSent 
  [S "This", (phrase section_), S "provides the", (phrase functional),
  plural requirement `sC` S "the business",
  S "tasks that the", (phrase software), S "is expected to complete, and the",
  (phrase nonfunctional), (plural requirement `sC` S "the qualities that the"), 
  (phrase software), S "is expected to exhibit"]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1_list :: Contents

s5_1 = SRS.funcReq [s5_1_list] []

s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
  s5_1_req7, s5_1_req8 :: Sentence

-- some requirements look like they could be parametrized
s5_1_req1 = foldlSent [S "Create a", (phrase $ space ^. term), S "for all of the",
  (plural $ rigidBody ^. term), S "in the", (phrase physicalSim), 
  S "to interact in"]

s5_1_req2 = foldlSent [S "Input the initial", 
  (plural $ mass ^. term) `sC` (plural $ vel ^. term) `sC` 
  (plural $ orientation ^. term) `sC` (plural $ angVel ^. term), 
  S "of" `sC` S "and", (plural $ force ^. term), S "applied on", 
  (plural $ rigidBody ^. term)]

s5_1_req3 = foldlSent [S "Input the", (phrase $ surface ^. term), 
  (plural property), S "of the bodies, such as", (phrase $ friction ^. term), 
  S "or", (phrase $ elasticity ^. term)]

s5_1_req4 = foldlSent [S "Verify that the inputs", 
  S "satisfy the required", plural physicalConstraint]

s5_1_req5 = foldlSent 
  [S "Determine the", (plural $ position ^. term), S "and", (plural $ vel ^. term), 
  S "over a", S "period of", (phrase $ time ^. term), S "of the", (getAcc twoD),
  (plural $ rigidBody ^. term), S "acted upon by a", (phrase $ force ^. term)]

s5_1_req6 = foldlSent
  [S "Determine the", (plural $ orientation ^. term), S "and", 
  (plural $ angVel ^. term), S "over a period of", (phrase $ time ^. term),
   S "of the", (getAcc twoD), (plural $ rigidBody ^. term)]

s5_1_req7 = foldlSent [S "Determine if any of the", (plural $ rigidBody ^. term), 
  S "in the", (phrase $ space ^. term), S "have collided"]

s5_1_req8 = foldlSent
  [S "Determine the", (plural $ position ^. term), S "and", (plural $ vel ^. term), 
  S "over a", S "period of", (phrase $ time ^. term), S "of the", (getAcc twoD), 
  (plural $ rigidBody ^. term), S "that have undergone a", 
  (phrase $ collision ^. term)]

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
s5_1_list' :: [Sentence]
s5_1_list' = [s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
            s5_1_req7, s5_1_req8]

s5_1_list = Enumeration (Simple $ mkEnumAbbrevList 1 (getAcc requirement) s5_1_list')

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

s5_2 :: Section
s5_2_intro :: Contents

s5_2 = SRS.nonfuncReq [s5_2_intro] []

s5_2_intro = Paragraph $ foldlSent 
  [(titleize' game), S "are resource intensive, so performance",
  S "is a high priority. Other", (phrase nonfunctional), plural requirement,
  S "that are a",
  S "priority are: correctness, understandability, portability,",
  S "reliability, and maintainability"]

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6_intro, s6_list :: Contents

s6 = SRS.likeChg [s6_intro, s6_list] []

s6_intro = Paragraph $ foldlSent [S "This", (phrase section_), S "lists the", 
  (plural likelyChg), S "to be made to the", (phrase physics), (phrase game), 
  (phrase library)]

s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3, 
  s6_likelyChg_stmt4 :: Sentence

--these statements look like they could be parametrized
s6_likelyChg_stmt1 = foldlSent [S "The internal", (getAcc ode) :+: 
  S "-solving algorithm used by the", (phrase library), 
  S "may change in the future"]

s6_likelyChg_stmt2 = foldlSent [S "The", (phrase library), S "may be",
  S "expanded to deal with edge-to-edge and vertex-to-vertex",
  (plural (collision ^. term))]

s6_likelyChg_stmt3 = foldlSent [S "The", (phrase library), S "may be", 
  S "expanded to include motion with damping"]

s6_likelyChg_stmt4 = foldlSent [S "The", (phrase library), S "may be",
  S "expanded to include joints and", (plural constraint)]

s6_list' :: [Sentence]
s6_list' = [s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3,
                    s6_likelyChg_stmt4]

s6_list = Enumeration (Simple $ mkEnumAbbrevList 1 (getAcc likelyChg) s6_list')


-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

s7 :: Section
s7_intro, s7_2dlist, s7_mid, s7_3dlist :: Contents

s7 = SRS.offShelfSol [s7_intro, s7_2dlist,
  s7_mid, s7_3dlist] []

s7_intro = Paragraph $ S "As mentioned in" +:+. ((makeRef s4_1) `sC`
  S "there already exist free" +:+ (phrase openSource) +:+ (phrase game) +:+
  (plural $ physLib ^. term)) +:+ S "Similar" +:+ (getAcc twoD) +:+ 
  (plural $ physLib ^. term) +:+ S "are:"

s7_2dlist = Enumeration (Bullet [
  Flat (S "Box2D: http://box2d.org/"),
  Flat (S "Nape" +:+ (titleize physics) +:+ S "Engine: http://napephys.com/")])

s7_mid = Paragraph $ foldl (+:+) (EmptyS) [S "Free", (phrase openSource), 
        S "3D", (phrase game), (plural $ physLib ^. term), S "include:"]

s7_3dlist = Enumeration (Bullet $ map (Flat) [
  (S "Bullet: http://bulletphysics.org/"),
  (S "Open Dynamics Engine: http://www.ode.org/"),
  (S "Newton" +:+ (titleize game) +:+ S "Dynamics: http://newtondynamics.com/")])

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------

s8 :: Section
s8 = SRS.traceyMandG [s8_intro1,s8_table1,s8_table2,s8_table3] []

s8_intro1 :: Contents
s8_intro1 = Paragraph $ foldlSent [S "The", (phrase purpose), S "of", 
  (plural traceyMatrix), S "is", S "to provide easy", (plural reference), 
  S "on what has to be additionally modified if",
  S "a certain", (phrase component), S "is changed. Every time a", 
  (phrase component), S "is changed,", S "the items in the column of that", 
  (phrase component), S "that are marked with an \"X\"", 
  S "should be modified as well. Table 3 shows the dependencies of", 
  (plural goalStmt) `sC` (plural requirement) `sC` (plural inModel) `sC` S "and",
  (plural datumConstraint), S "with each other.", 
  S "Table 4 shows the dependencies of", (plural thModel) `sC` (plural genDefn) `sC`
  (plural dataDefn) `sC` S "and", (plural inModel), 
  S "on the assumptions. Finally, Table 5",
  S "shows the dependencies of the", (plural thModel) `sC` (plural genDefn) `sC` 
  (plural dataDefn) `sC` S "and", (plural inModel), S "on each other"]


s8_row_t1, s8_colString_t1 :: [String]
s8_row_t1 = ["IM1", "IM2", "IM3", "R1", "R4", "R7", "Data Constraints"]
s8_colString_t1 = ["GS1", "GS2", "GS3", "GS4", "R1", "R2", "R3", "R4", "R5",
  "R6", "R7", "R8"]
s8_colName_t1 :: [Sentence]
s8_colName_t1 = map (S) s8_colString_t1

gS1_t1, gS2_t1, gS3_t1, gS4_t1, r1_t1, r2_t1, r3_t1, r4_t1, r5_t1, r6_t1, r7_t1, 
  r8_t1 :: [String]
gS1_t1 = ["IM1"]
gS2_t1 = ["IM2"]
gS3_t1 = ["IM3"]
gS4_t1 = ["IM3", "R7"]
r1_t1 = []
r2_t1 = ["IM1", "IM2", "R4"]
r3_t1 = ["IM3", "R4"]
r4_t1 = ["Data Constraints"]
r5_t1 = ["IM1"]
r6_t1 = ["IM2"]
r7_t1 = ["R1"]
r8_t1 = ["IM3", "R7"]

s8_columns_t1 :: [[String]]
s8_columns_t1 = [gS1_t1, gS2_t1, gS3_t1, gS4_t1, r1_t1, r2_t1, r3_t1, r4_t1, 
  r5_t1, r6_t1, r7_t1, r8_t1]

s8_table1 :: Contents
s8_table1 = Table (EmptyS:(map (S) s8_row_t1))
  (makeTMatrix s8_colName_t1 s8_columns_t1 s8_row_t1)
  ((titleize traceyMatrix) +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ titleize' requirement +:+
  S "and Other" +:+ titleize' item) True

s8_row_t2 :: [String]
s8_row_t2 = ["A1", "A2", "A3", "A4", "A5", "A6", "A7"]
s8_colString_t2 :: [String]
s8_colString_t2 = ["T1", "T2", "T3", "T4", "T5", "GD1", "GD2", 
  "GD3", "GD4", "GD5", "GD6", "GD7", "DD1", "DD2", "DD3",
  "DD4", "DD5", "DD6", "DD7", "DD8", "IM1", "IM2", "IM3", 
  "LC1", "LC2", "LC3", "LC4"]

s8_colName_t2 :: [Sentence]
s8_colName_t2 = map (S) s8_colString_t2

s8_columns_t2 :: [[String]]
s8_columns_t2 = [t1_t2, t2_t2, t3_t2, t4_t2, t5_t2, gD1_t2, gD2_t2, gD3_t2,
  gD4_t2, gD5_t2, gD6_t2, gD7_t2, dD1_t2, dD2_t2, dD3_t2, dD4_t2, dD5_t2, dD6_t2,
  dD7_t2, dD8_t2, iM1_t2, iM2_t2, iM3_t2, lC1, lC2, lC3, lC4]

t1_t2, t2_t2, t3_t2, t4_t2, t5_t2, gD1_t2, gD2_t2, gD3_t2, gD4_t2, gD5_t2, 
  gD6_t2, gD7_t2, dD1_t2, dD2_t2, dD3_t2, dD4_t2, dD5_t2, dD6_t2, dD7_t2, dD8_t2, 
  iM1_t2, iM2_t2, iM3_t2, lC1, lC2, lC3, lC4 :: [String]
t1_t2 = []
t2_t2 = []
t3_t2 = []
t4_t2 = ["A1"]
t5_t2 = []
gD1_t2 = []
gD2_t2 = []
gD3_t2 = ["A2","A3"]
gD4_t2 = []
gD5_t2 = []
gD6_t2 = []
gD7_t2 = []
dD1_t2 = ["A1","A2"]
dD2_t2 = ["A1","A2","A6"]
dD3_t2 = ["A1","A2","A6"]
dD4_t2 = ["A1","A2","A6"]
dD5_t2 = ["A1","A2","A6"]
dD6_t2 = ["A1","A2","A6"]
dD7_t2 = ["A1","A2","A6"]
dD8_t2 = ["A1","A2","A4","A5"]
iM1_t2 = ["A1","A2","A6","A7"]
iM2_t2 = ["A1","A2","A4","A6","A7"]
iM3_t2 = ["A1","A2","A5","A6","A7"]
lC1 = []
lC2 = ["A5"]
lC3 = ["A6"]
lC4 = ["A7"]

s8_table2 :: Contents
s8_table2 = Table (EmptyS:(map (S) s8_row_t2))
  (makeTMatrix s8_colName_t2 s8_columns_t2 s8_row_t2)
  ((titleize traceyMatrix) +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ titleize' assumption +:+
  S "and Other" +:+ titleize' item) True

s8_row_t3, s8_colString_t3 :: [String]
s8_row_t3 = ["T1","T2","T3","T4","T5","GD1","GD2","GD3","GD4","GD5","GD6","GD7",
  "DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8", "IM1", "IM2", 
  "IM3"]

s8_colString_t3 = s8_row_t3
s8_colName_t3 :: [Sentence]
s8_colName_t3 = map (S) s8_colString_t3

s8_columns_t3 :: [[String]]
s8_columns_t3 = [t1_t3, t2_t3, t3_t3, t4_t3, t5_t3, gD1_t3, gD2_t3, gD3_t3, 
  gD4_t3, gD5_t3, gD6_t3, gD7_t3, dD1_t3, dD2_t3, dD3_t3, dD4_t3, dD5_t3, dD6_t3,
  dD7_t3, dD8_t3, iM1_t3, iM2_t3, iM3_t3]

t1_t3, t2_t3, t3_t3, t4_t3, t5_t3, gD1_t3, gD2_t3, gD3_t3, gD4_t3, gD5_t3, gD6_t3,
  gD7_t3, dD1_t3, dD2_t3, dD3_t3, dD4_t3, dD5_t3, dD6_t3, dD7_t3, dD8_t3, iM1_t3,
  iM2_t3, iM3_t3 :: [String]

t1_t3 = [] 
t2_t3 = []
t3_t3 = []
t4_t3 = []
t5_t3 = ["GD6", "GD7"]
gD1_t3 = ["T1"]
gD2_t3 = ["T2", "GD1"]
gD3_t3 = ["T1", "T3"]
gD4_t3 = []
gD5_t3 = ["GD4"]
gD6_t3 = []
gD7_t3 = []
dD1_t3 = []
dD2_t3 = []
dD3_t3 = []
dD4_t3 = []
dD5_t3 = []
dD6_t3 = []
dD7_t3 = []
dD8_t3 = ["T4", "GD1","GD4","GD5","GD7","IM3"]
iM1_t3 = ["T1", "GD3", "DD1","DD2","DD3","DD4"]
iM2_t3 = ["T5", "DD1", "DD2", "DD3", "DD4"]
iM3_t3 = ["GD1", "GD2", "GD6", "GD7", "DD1", "DD8"]

s8_table3 :: Contents
s8_table3 = Table (EmptyS:(map (S) s8_row_t3))
  (makeTMatrix s8_colName_t3 s8_columns_t3 s8_row_t3)
  ((titleize traceyMatrix) +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ titleize' item +:+
  S "and Other" +:+ titleize' section_) True
----------------
-- REFERENCES --
----------------

-- To be added --