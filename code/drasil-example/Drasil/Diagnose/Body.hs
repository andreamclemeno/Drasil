module Drasil.Diagnose.Body where

import Language.Drasil hiding (Symbol(..), Vector)
import Language.Drasil.Code (relToQD)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _definitions, _configFiles, _defSequence, _inputs, _kind, 
  _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
  
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil

import Drasil.DocLang --(SRSDecl, mkDoc)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Documentation (analysis, doccon, doccon', physics,
  problem, srsDomains)
import Data.Drasil.Concepts.Math (cartesian, mathcon)

import Data.Drasil.People (amclemeno)
import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Software (program)
import Drasil.Diagnose.Concepts
import Drasil.Diagnose.Figures (figVirusinbody)
import Drasil.Diagnose.Concepts (diagnoseTitle, virus, viralloaddef, infectedcells, helperTcell, elimination, aids, diagnosis, progression)
import Drasil.Diagnose.Goals (goals)
import Drasil.Diagnose.Assumptions (assumptions)
import Drasil.Diagnose.TMods 
import Drasil.Diagnose.IMods 
import Drasil.Diagnose.Unitals
import Drasil.Diagnose.References
import Drasil.Diagnose.Requirements (funcReqs, nonfuncReqs)
import Drasil.Diagnose.GenDefs (genDefns)
import Drasil.Diagnose.DataDefs (dataDefs)
import Drasil.Diagnose.Changes

import Data.Drasil.Quantities.Physics (iVel, physicscon)
import Data.Drasil.Concepts.Physics (constAccel, gravity, physicCon, physicCon',
  rectilinear, twoD)
import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.PhysicalProperties (mass, dimension, concent, physicalcon, materialProprty)

import Control.Lens ((^.))
import Theory.Drasil (Theory(defined_fun, defined_quant))


import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation),
  DocSection(AuxConstntSec, Bibliography, IntroSec, RefSec, ReqrmntSec, SSDSec, LCsSec, UCsSec, TraceabilitySec), 
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(IntroProg), IntroSub(IScope), ProblemDescription(PDProg), PDSub(..),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SRSDecl,
  SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  TConvention(..), TSIntro(..), TraceabilitySec(TraceabilityProg),
  Verbosity(Verbose), intro, mkDoc, traceMatStandard, tsymb)


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
    IntroProg justification (phrase diagnoseTitle)
      [ IScope scope ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc diagnoseTitle physSystParts figVirusinbody []
        , Goals goalsInputs]       
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub EmptyS []
      , NonFReqsSub
      ],
  LCsSec, 
  UCsSec,     
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
 AuxConstntSec $
    AuxConsProg diagnoseTitle constants,
  Bibliography
  ]
  
justification :: Sentence 
justification = foldlSent [S "HIV-1 is a virus that attacks cells of the immune system needed to fight off diseases.",
  S "The virus leads to an uncurable disease called AIDs. Therefore, it is useful to have a",
  phrase program, S "to model these types of" +:+. plural problem,
  S "The", phrase program, S "documented here is called", phrase diagnoseTitle]
scope :: Sentence
scope = foldlSent_ [S "the analysis of HIV-1 concentration over time"]

si :: SystemInformation
si = SI {
  _sys         = diagnoseTitle,
  _kind        = Doc.srs,
  _authors     = [amclemeno],
  _purpose     = [],
  _quants      = symbols, -- [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = map (relToQD symbMap) iMods ++ 
                 concatMap (^. defined_quant) tMods ++
                 concatMap (^. defined_fun) tMods, --[] :: [QDefinition],
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs, --[QuantityDict],
  _outputs     = outputs, -- [QuantityDict],
  _defSequence = [] :: [Block QDefinition],
  _constraints = inConstraints, -- [ConstrainedChunk],
  _constants   = constants, --[QDefinition],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

------
  
  
symbMap :: ChunkDB
symbMap = cdb (map qw physicscon ++ symbols) (nw diagnoseTitle : nw mass : nw inValue : [nw program] ++
    map nw doccon ++ map nw doccon' ++ map nw physicalcon ++ map nw physicCon ++ map nw physicCon'++ map nw acronyms ++
    map nw mathcon ++ map nw fundamentals ++ map nw derived ++ map nw tMCC) (map cw defSymbols ++ srsDomains)
  (siUnits) (dataDefs) (iMods)
  (genDefns) (tMods) (concIns)
  ([] :: [Section]) ([] :: [LabelledContent])
  
-- add ++ map nw unitless in second arg if there is unitlessconstants
 
usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ( map nw physicalcon ++ map nw tMCC ++ map nw acronyms ++ map nw symbols) 
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])
  

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumptions ++ funcReqs ++ goals ++ nonfuncReqs ++ likelyChgs ++ unlikelyChgs

-------------------------
-- Problem Description --
-------------------------

prob :: Sentence
prob = foldlSent_ [S "assess the risk before substantial immune destruction has occurred."
  , S "The system will predict viral load at 30 days and the patient's progression"]
  
---------------------------------
-- Terminology and Definitions --
---------------------------------
terms :: [ConceptChunk]
terms = [virus, viralloaddef, infectedcells, helperTcell, elimination, aids, diagnosis, progression]


---------------------------------
-- Physical System Description --
---------------------------------

physSystParts :: [Sentence]
physSystParts = map foldlSent [
  [S "The HIV Virion"],
  [S "The virus-infected cells"],
  [S "The Helper T cell"],
  [S "The Human Body"]]
  
---------------------------------
-- Goal Statements --
---------------------------------

goalsInputs :: [Sentence]
goalsInputs = [S "two HIV-1 viral load datum taken on consecutive days"]
