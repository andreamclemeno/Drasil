module Language.Drasil.Code.Imperative.Descriptions (
  modDesc, inputParametersDesc, inputConstructorDesc, inputFormatDesc, 
  derivedValuesDesc, inputConstraintsDesc, constModDesc, outputFormatDesc, 
  inputClassDesc, constClassDesc, inFmtFuncDesc, inConsFuncDesc, dvFuncDesc, 
  calcModDesc, woFuncDesc
) where

import Utils.Drasil (stringList)

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..), inMod)
import Language.Drasil.Chunk.Code (CodeIdea(codeName))
import Language.Drasil.CodeSpec (CodeSpec(..), InputModule(..), Structure(..))

import Data.Map (member)
import qualified Data.Map as Map (filter, lookup, elems)
import Control.Monad.Reader (Reader, ask)

modDesc :: Reader DrasilState [String] -> Reader DrasilState String
modDesc = fmap ((++) "Provides " . stringList)

inputParametersDesc :: Reader DrasilState [String]
inputParametersDesc = do
  g <- ask
  ifDesc <- inputFormatDesc
  dvDesc <- derivedValuesDesc
  icDesc <- inputConstraintsDesc
  let im = inMod g
      st = inStruct g
      ipDesc Separated = inDesc st
      ipDesc Combined = inDesc st ++ [ifDesc, dvDesc, icDesc]
      inDesc Bundled = ["the structure for holding input values"]
      inDesc Unbundled = [""]
  return $ ipDesc im

inputConstructorDesc :: Reader DrasilState String
inputConstructorDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let ifDesc False = ""
      ifDesc True = "reading inputs"
      idDesc False = ""
      idDesc True = "calculating derived values"
      icDesc False = ""
      icDesc True = "checking " ++ pAndS ++ " on the input"
      dl = defList g
  return $ "Initializes input object by " ++ stringList [ 
    ifDesc ("get_input" `elem` dl),
    idDesc ("derived_values" `elem` dl),
    icDesc ("input_constraints" `elem` dl)]

inputFormatDesc :: Reader DrasilState String
inputFormatDesc = do
  g <- ask
  let ifDesc Nothing = ""
      ifDesc _ = "the function for reading inputs"
  return $ ifDesc $ Map.lookup "get_input" (eMap g)

derivedValuesDesc :: Reader DrasilState String
derivedValuesDesc = do
  g <- ask
  let dvDesc Nothing = ""
      dvDesc _ = "the function for calculating derived values"
  return $ dvDesc $ Map.lookup "derived_values" (eMap g)

inputConstraintsDesc :: Reader DrasilState String
inputConstraintsDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc Nothing = ""
      icDesc _ = "the function for checking the " ++ pAndS ++ 
        " on the input"
  return $ icDesc $ Map.lookup "input_constraints" (eMap g)

constModDesc :: Reader DrasilState String
constModDesc = do
  g <- ask
  let cDesc [] = ""
      cDesc _ = "the structure for holding constant values"
  return $ cDesc $ filter (flip member (eMap g) . codeName) 
    (constants $ codeSpec g)

outputFormatDesc :: Reader DrasilState String
outputFormatDesc = do
  g <- ask
  let ofDesc Nothing = ""
      ofDesc _ = "the function for writing outputs"
  return $ ofDesc $ Map.lookup "write_output" (eMap g)

inputClassDesc :: Reader DrasilState String
inputClassDesc = do
  g <- ask
  let cname = "InputParameters"
      inClassD [] = ""
      inClassD _ = "Structure for holding the " ++ stringList [
        inPs $ extInputs $ codeSpec g,
        dVs $ "derived_values" `elem` defList g,
        cVs $ filter (flip member (Map.filter (cname ==) 
          (eMap g)) . codeName) (constants $ codeSpec g)]
      inPs [] = ""
      inPs _ = "input values"
      dVs False = ""
      dVs _ = "derived values"
      cVs [] = ""
      cVs _ = "constant values"
  return $ inClassD $ inputs $ codeSpec g

constClassDesc :: Reader DrasilState String
constClassDesc = do
  g <- ask
  let ccDesc [] = ""
      ccDesc _ = "Structure for holding the constant values"
  return $ ccDesc $ filter (flip member (eMap g) . codeName) 
    (constants $ codeSpec g)

inFmtFuncDesc :: Reader DrasilState String
inFmtFuncDesc = do
  g <- ask
  let ifDesc False = ""
      ifDesc _ = "Reads input from a file with the given file name"
  return $ ifDesc $ "get_input" `elem` defList g

inConsFuncDesc :: Reader DrasilState String
inConsFuncDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc False = ""
      icDesc _ = "Verifies that input values satisfy the " ++ pAndS
  return $ icDesc $ "input_constraints" `elem` defList g

dvFuncDesc :: Reader DrasilState String
dvFuncDesc = do
  g <- ask
  let dvDesc False = ""
      dvDesc _ = "Calculates values that can be immediately derived from the" ++
        " inputs"
  return $ dvDesc $ "derived_values" `elem` defList g

calcModDesc :: String
calcModDesc = "Provides functions for calculating the outputs"

woFuncDesc :: Reader DrasilState String
woFuncDesc = do
  g <- ask
  let woDesc False = ""
      woDesc _ = "Writes the output values to output.txt"
  return $ woDesc $ "write_output" `elem` defList g

physAndSfwrCons :: Reader DrasilState String
physAndSfwrCons = do
  g <- ask
  let cns = concat $ Map.elems (cMap $ codeSpec g)
  return $ stringList [
    if null (map isPhysC cns) then "" else "physical constraints",
    if null (map isSfwrC cns) then "" else "software constraints"]