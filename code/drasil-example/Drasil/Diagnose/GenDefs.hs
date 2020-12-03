module Drasil.Diagnose.GenDefs (genDefns, vLoadtGD) where

import Prelude hiding (exp)
import Language.Drasil
import Theory.Drasil (GenDefn, TheoryModel, gd, gdNoRefs)
import Utils.Drasil

import Drasil.Diagnose.Assumptions 
import Drasil.Diagnose.TMods (expElimTM)
import Drasil.Diagnose.Unitals 
import Drasil.Diagnose.References

genDefns :: [GenDefn]
genDefns = [vLoadtGD]

----------
vLoadtGD :: GenDefn
vLoadtGD = gd vLoadtRC (getUnit vLoad) (Just vLoadtDeriv)
  [makeCite hobbie1970] "vLoadt" [{-Notes-}]


vLoadtRC :: RelationConcept
vLoadtRC = makeRC "vLoadtRC" (nounPhraseSent $ foldlSent_ 
            [S "VLoadt as a function" `sOf` phrase time, S "for constant decay rate"])
            EmptyS $ sy vLoadt $= sy vLoado * exp (negate(sy elimConst * sy time))

vLoadtDeriv :: Derivation
vLoadtDeriv = mkDerivName (phrase vLoadt) (weave [vLoadtDerivSents, map E vLoadtDerivEqns])

vLoadtDerivSents :: [Sentence]
vLoadtDerivSents = [vLoadtDerivSent1, vLoadtDerivSent2, vLoadtDerivSent3]
                             
vLoadtDerivSent1, vLoadtDerivSent2, vLoadtDerivSent3 :: Sentence

vLoadtDerivSent1 = foldlSentCol [S "Using the First-Order rate Law" `sIn` makeRef2S expElimTM `sC`
                                 S "we have" ]
                                 
vLoadtDerivSent2 = foldlSentCol [S "Where", ch vLoadt +:+ S "denotes the", phrase vLoadt `sC`
                                    ch vLoado +:+ S "denotes the", phrase vLoado `sAnd` ch elimConst +:+ 
                                    S "denotes the", phrase elimConst :+: 
                                    S ". When rearranging for integration" `sC` S " we have"]
                 
vLoadtDerivSent3 = foldlSentCol [S "Performing the integration" `sC` S "we have the required equation"]
                                

vLoadtDerivEqns :: [Expr]
vLoadtDerivEqns = [vLoadtDerivEqn1, vLoadtDerivEqn2, vLoadtDerivEqn3]

vLoadtDerivEqn1, vLoadtDerivEqn2, vLoadtDerivEqn3 :: Expr
vLoadtDerivEqn1 = deriv (sy vLoad) time $= negate(sy elimConst * sy vLoado)
vLoadtDerivEqn2 = defint (eqSymb vLoadt) (sy vLoado) (sy vLoadt) 1 $= 
                  negate (defint (eqSymb time) 0 (sy time) (sy elimConst))
vLoadtDerivEqn3 = sy vLoadt $= sy vLoado * exp (negate(sy elimConst * sy time))
                  



           
                   
                   
                   
                   
                   
                   
                   
                   
                   
