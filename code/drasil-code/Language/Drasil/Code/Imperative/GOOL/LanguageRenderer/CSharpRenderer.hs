{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (
  CSharpProject(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName, 
  makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, nativeBinary, osClassDefault)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import GOOL.Drasil (liftList)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Prelude as P ((<>))
import Text.PrettyPrint.HughesPJ (Doc, text)

newtype CSharpProject a = CSP {unCSP :: a}

instance Functor CSharpProject where
  fmap f (CSP x) = CSP (f x)

instance Applicative CSharpProject where
  pure = CSP
  (CSP f) <*> (CSP x) = CSP (f x)

instance Monad CSharpProject where
  return = CSP
  CSP x >>= f = f x

instance PackageSym CSharpProject where
  type Package CSharpProject = PackData
  package p = liftList (packD p)

instance AuxiliarySym CSharpProject where
  type Auxiliary CSharpProject = AuxData
  type AuxHelper CSharpProject = Doc
  doxConfig pName p = fmap (ad doxConfigName . makeDoxConfig pName p)
    optimizeDox
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "NO"

  makefile cms p = return $ ad makefileName (makeBuild cms csBuildConfig 
    csRunnable p)

csBuildConfig :: Maybe BuildConfig
csBuildConfig = buildAll $ \i o -> [osClassDefault "CSC" "csc" "mcs", 
  asFragment "-out:" P.<> o] ++ i

csRunnable :: Runnable
csRunnable = nativeBinary