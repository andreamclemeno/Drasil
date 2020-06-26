module Language.Drasil.Code.Imperative.GenODE (
  chooseODELib
) where

import Language.Drasil.Code.ExtLibImport (ExtLibState(..), 
  genExternalLibraryCall)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Chunk.Code (codeName)
import Language.Drasil.Chunk.CodeDefinition (odeDef)
import Language.Drasil.Mod (Name, Version)
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Data.ODELibPckg (ODELibPckg(..))

import Control.Monad.State (State, modify)
import Text.PrettyPrint.HughesPJ (Doc, ($$), text, empty)

type ODEGenInfo = (Maybe FilePath, [(Name, ExtLibState)], (Name,Version))

-- Chooses the first ODELibPckg from the list specified by the user that is 
-- compatible with the current target Lang. 
-- Interprets the ExternalLibrary and ExternalLibraryCall for the selected 
-- ODELibPckg by concretizing the ExternalLibraryCall with each of the ODEInfos
chooseODELib :: Lang -> [ODELibPckg] -> [ODEInfo] -> State Doc ODEGenInfo
chooseODELib _ _ [] = return (Nothing, [], ("",""))
chooseODELib l olps odes = chooseODELib' olps olps
  where chooseODELib' :: [ODELibPckg] -> [ODELibPckg]->State Doc ODEGenInfo
        chooseODELib' _ [] = error $ "None of the chosen ODE libraries are " ++ 
          "compatible with " ++ show l
        chooseODELib' initLib (o:os) = if l `elem` compatibleLangs o 
          then do 
            modify ($$ firstChoiceODELib initLib o)
            return (libPath o, map (\ode -> (codeName $ odeDef ode, 
              genExternalLibraryCall (libSpec o) $ libCall o ode)) odes, 
                (libName o, libVers o)) 
          else modify ($$ incompatibleLib l o) >> chooseODELib' initLib os

-- Defines a design log message based on an incompatibility between the given 
-- Lang and chosen ODELibPckg.
incompatibleLib :: Lang -> ODELibPckg -> Doc
incompatibleLib lng lib = text $ "Language " ++ show lng ++ " is not " ++ 
  "compatible with chosen library " ++ libName lib ++ ", trying next choice." 

firstChoiceODELib :: [ODELibPckg] -> ODELibPckg -> Doc
firstChoiceODELib prefer olp =  if libName (head prefer) == libName olp  then 
  text $ "Successfully " ++"selected first choice ODELibPckg "++ 
  libName olp  else empty