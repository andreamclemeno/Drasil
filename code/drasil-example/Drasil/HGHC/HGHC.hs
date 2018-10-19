module Drasil.HGHC.HGHC (srsBody, thisCode, allSymbols, printSetting) where

import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Language.Drasil.Code (CodeSpec, codeSpec)
import Drasil.DocLang (DocSection(RefSec, SSDSec), Literature(Lit, Manual), 
    RefSec(..), RefTab(TUnits), TSIntro(SymbConvention, TSPurpose), DocDesc, 
    intro, mkDoc, tsymb, InclUnits(IncludeUnits), Verbosity(Verbose),
    Field(DefiningEquation, Description, Label, Symbol, Units), SolChSpec(SCSProg), 
    SCSSub(DDs), DerivationDisplay(HideDerivation), SSDSub(SSDSolChSpec), 
    SSDSec(SSDProg))

import Drasil.HGHC.HeatTransfer (fp, hghc, hghcVarsDD, htInputs, htOutputs, 
    nuclearPhys, symbols)

import Data.Drasil.SI_Units (si_units)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (srs)
import Data.Drasil.Phrase (for)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)

thisCode :: CodeSpec
thisCode = codeSpec thisSI []
  
thisSI :: SystemInformation
thisSI = SI {
  _sys = hghc,
  _kind = srs,
  _authors = [spencerSmith],
  _units = check_si,  
  _quants = symbols,
  _concepts = ([] :: [UnitaryConceptDict]),
  _definitions = ([] :: [QDefinition]),
  _datadefs = hghcVarsDD,
  _inputs = htInputs,
  _outputs = htOutputs,
  _defSequence = ([] :: [Block QDefinition]),
  _constraints = ([] :: [ConstrainedChunk]),
  _constants = [],
  _sysinfodb = allSymbols,
  _refdb = rdb [] [] [] [] [] [] -- FIXME?
}

check_si :: [UnitDefn]
check_si = collectUnits allSymbols symbols 

allSymbols :: ChunkDB
allSymbols = cdb symbols (map nw symbols) ([] :: [ConceptChunk]) -- FIXME: Fill in concepts
  si_units

printSetting :: PrintingInformation
printSetting = PI allSymbols defaultConfiguration
  
thisSRS :: DocDesc
thisSRS = RefSec (RefProg intro 
  [TUnits, 
  tsymb [TSPurpose, SymbConvention [Lit (nw nuclearPhys), Manual (nw fp)]]]) : 
  [SSDSec ( SSDProg [SSDSolChSpec 
  (SCSProg [DDs [Label, Symbol, Units, DefiningEquation,
  Description Verbose IncludeUnits] hghcVarsDD HideDerivation]) ] ) ]
  
srsBody :: Document
srsBody = mkDoc thisSRS for thisSI
