module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Language.Drasil.ShortHands (lM, cL, cV, cN, cA, cB, cC, lGamma, lRho)

import Data.Drasil.Concepts.PhysicalProperties as CPP (density, specWeight, len,
  mass, vol, concent)
import Data.Drasil.SI_Units (kilogram, metre, m_3, specificWeight, mole)
import Data.Drasil.Units.PhysicalProperties (densityU, concentU)

density, specWeight, mass, len, vol, amt, amta, amtb, concent :: UnitalChunk
density       = uc CPP.density       lRho   densityU
specWeight    = uc CPP.specWeight    lGamma specificWeight
mass          = uc CPP.mass          lM     kilogram
len           = uc CPP.len           cL     metre
vol           = uc CPP.vol           cV     m_3
amt           = uc amt               cN     mole
amta          = uc amta              cA     mole
amtb          = uc amtb              cB     mole
concent       = uc CPP.concent       cC     concentU