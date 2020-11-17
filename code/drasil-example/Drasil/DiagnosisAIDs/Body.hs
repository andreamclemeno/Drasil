module Drasil.DiagnosisAIDs.Body where

import Language.Drasil hiding (Symbol(..), Vector)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _configFiles, _definitions, _defSequence, _inputs, _kind, _outputs, 
  _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil

import Data.Drasil.People (amclemeno)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Concepts.Documentation (doccon)
--import Drasil.DiagnosisAIDs.Figures (figVirusbody)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Drasil.DocLang.SRS
import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DocSection(AuxConstntSec, Bibliography, RefSec),
  Emphasis(Bold), RefSec(..), RefTab(..),
  TConvention(..), TSIntro(..), intro,tsymb, SRSDecl, mkDoc, Sentence)
  


srs :: Document
srs = mkDoc mkSRS (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [RefSec $      --This creates the Reference section of the SRS
    RefProg intro      -- This add the introduction blob to the reference section  
      [ TUnits         -- Adds table of unit section with a table frame
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits] -- Adds table of symbol section with a table frame
      --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
      , TAandA         -- Add table of abbreviation and acronym section
      ],
   IntroSec $
     IntroProg justification (phrase diagnosisAIDstitle)
       [ IScope scope ],
   SSDSec $ SSDProg
       [ SSDProblem $ PDProg prob []
         [ TermsAndDefs Nothing terms
         , PhySysDesc diagnosisaids physSystParts figVirusbody []
         , Goals goalsInputs]
       , SSDSolChSpec $ SCSProg
  --        [ Assumptions
  --          , TMs [] (Label : stdFields)
  --       , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
  --       , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
  --     --  , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
  --     --  , Constraints EmptyS inConstraints
  --     --  , CorrSolnPpties outConstraints []
  --      ]
  --    ],
  --ReqrmntSec $
  --  ReqsProg
  --     [ FReqsSub EmptyS []
  --     , NonFReqsSub
  --     ],
  -- TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
--    AuxConstntSec $
--     AuxConsProg DiagnosisAIDs [],  --Adds Auxilliary constraint section
  Bibliography                    -- Adds reference section
  ]

justification, scope :: Sentence
justification = foldlSent  [S "diagnosisAIDstitle" +:+. S "diagnosisAIDstitle",
  phrase diagnosisAIDstitle]
scope         = foldlSent_ [S "diagnosisAIDstitle is the subject" +:+. S"diagnosisAIDstitle is the focus", phrase diagnosisAIDstitle]

si :: SystemInformation
si = SI {
  _sys         = diagnosisAIDstitle,
  _kind        = Doc.srs,
  _authors     = [amclemeno],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = [] :: [DataDefinition],
  _configFiles  = [],
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _defSequence = [] :: [Block QDefinition],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [QDefinition],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
  refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) (nw diagnosisAIDstitle) [nw program] 
  (nw diagnosisAIDstitle: [nw program] ++ map nw doccon ++ map nw doccon’)([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

--stdFields :: Fields
--stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb [] []

-- MOVE TO CONCEPTS

diagnosisAIDstitle :: CI
diagnosisAIDstitle = commonIdeaWithDict "diagnosisAIDstitle" (pn "DiagnosisAIDs") "DiagnosisAIDs" []

-- MOVE TO DATA.PEOPLE
--authorName :: Person
--authorName = person "Author" "Name"

------------------------------------
--Problem Description
------------------------------------

prob :: Sentence
prob = foldlSent_ [S "Problem Description" `sAnd` S "Problem Description"]

-- ---------------------------------
-- -- Terminology and Definitions --
-- ---------------------------------

terms :: [ConceptChunk]
terms = [viralload]


-- ---------------------------------
-- -- Physical System Description --
-- ---------------------------------

-- physSystParts :: [Sentence]
-- physSystParts = map foldlSent [
--   [S "The", phrase diagnosisAIDstitle],
--   [S "The", phrase diagnosisAIDstitle],
--   [S "The", phrase viralload]]

-- ------------------------------

goalsInputs :: [Sentence]
goalsInputs = [phrase diagnosisAIDstitle `ofThe` diagnosisAIDstitle ]-}
 

