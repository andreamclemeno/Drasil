module Data.Drasil.Units.PhysicalProperties where

import Data.Drasil.SI_Units (mole, kilogram, m_3)
import Language.Drasil (UnitDefn, newUnit, (/:))

densityU :: UnitDefn
densityU = newUnit "density"              $ kilogram /: m_3


concentU :: UnitDefn
concentU = newUnit "concent"              $ mole /: m_3
