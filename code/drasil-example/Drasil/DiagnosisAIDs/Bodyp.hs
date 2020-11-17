module Drasil.DiagnosisAIDs.Body (printSetting, si, srs) where

import Language.Drasil hiding (Symbol(..), Vector)
import Language.Drasil.Code (relToQD)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _configFiles, _definitions, _defSequence, _inputs, _kind, 
  _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil

import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation),
  DocSection(AuxConstntSec, Bibliography, IntroSec, RefSec, ReqrmntSec, SSDSec, TraceabilitySec),
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(IntroProg), IntroSub(IScope), ProblemDescription(PDProg), PDSub(..),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SRSDecl,
  SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  TConvention(..), TSIntro(..), TraceabilitySec(TraceabilityProg),
  Verbosity(Verbose), intro, mkDoc, traceMatStandard, tsymb)

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (analysis, doccon, doccon', physics,
  problem, srsDomains)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Math (cartesian, mathcon)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (constAccel, gravity, physicCon, physicCon',
  rectilinear, twoD)
import Data.Drasil.Concepts.Software (program)

--import Data.Drasil.Quantities.Math (pi_)
--import Data.Drasil.Quantities.Physics (iVel, physicscon)

import Data.Drasil.People (amclemeno
import Data.Drasil.SI_Units (metre, radian, second)

--import Drasil.Projectile.Assumptions (assumptions)
--import Drasil.Projectile.Concepts (concepts, projectileTitle, landingPos,
--  launcher, projectile, target)
--import Drasil.Projectile.DataDefs (dataDefs)
--import Drasil.Projectile.Figures (figLaunch)
--import Drasil.Projectile.GenDefs (genDefns)
--import Drasil.Projectile.Goals (goals)
--import Drasil.Projectile.IMods (iMods)
--import Drasil.Projectile.References (citations)
--import Drasil.Projectile.Requirements (funcReqs, nonfuncReqs)
--import Drasil.Projectile.TMods (tMods)
--import Drasil.Projectile.Unitals (acronyms, constants, constrained, inConstraints,
--  inputs, launAngle, outConstraints, outputs, symbols, unitalIdeas, unitalQuants)

srs :: Document
srs = mkDoc mkSRS (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IScope scope ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `ofThe` phrase projectile]]
--      , SSDSolChSpec $ SCSProg
--        [ Assumptions
--        , TMs [] (Label : stdFields)
--        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
--        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
--       , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
--        , Constraints EmptyS inConstraints
--        , CorrSolnPpties outConstraints []
--        ]
--      ],
--  ReqrmntSec $
--    ReqsProg
--      [ FReqsSub EmptyS []
--      , NonFReqsSub
--      ],
--  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
--  AuxConstntSec $
--    AuxConsProg projectileTitle constants,
  Bibliography
  ]

justification, scope :: Sentence
justification = foldlSent [atStart projectile, S "motion is a common" +:+.
  S "Therefore, it is useful to have a", phrase diagnosisAIDstitle]
scope = foldlSent_ [S "diagnosisAIDs",
		   ]

si :: SystemInformation
si = SI {
  _sys         = diagnosisAIDstitle,
  _kind        = Doc.srs,
  _authors     = [amclemeno],
  _purpose     = [],
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [QDefinition],
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = map cnstrw constrained,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) [nw diagnosisAIDstitle] ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])


usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (nw pi_ : map nw acronyms ++ map nw symbols)
  (cw pi_ : srsDomains) ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

--stdFields :: Fields
--stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

--refDB :: ReferenceDB
--refDB = rdb citations concIns

--concIns :: [ConceptInstance]
--concIns = assumptions ++ funcReqs ++ goals ++ nonfuncReqs

-- MOVE TO CONCEPTS

diagnosisAIDstitle :: CI
diagnosisAIDstitle = commonIdeaWithDict "diagnosisAIDstitle" (pn "DiagnosisAIDs") "DiagnosisAIDs" []

-- MOVE TO DATA.PEOPLE
--authorName :: Person
--authorName = person "Author" "Name"

-------------------------
-- Problem Description --
-------------------------

--prob :: Sentence
--prob = foldlSent_ [S "efficiently" `sAnd` S "correctly predict the",
--  phrase landingPos, S "of a", phrase projectile]

---------------------------------
-- Terminology and Definitions --
---------------------------------

--terms :: [ConceptChunk]
--terms = [launcher, projectile, target, gravity, cartesian, rectilinear]

---------------------------------
-- Physical System Description --
---------------------------------

--physSystParts :: [Sentence]
--physSystParts = map foldlSent [
--  [S "The", phrase launcher],
--  [S "The", phrase projectile, sParen (S "with" +:+ getTandS iVel `sAnd` getTandS launAngle)],
--  [S "The", phrase target]]
