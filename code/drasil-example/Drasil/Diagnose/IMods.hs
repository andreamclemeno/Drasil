module Drasil.Diagnose.IMods where

import Prelude hiding (exp)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC) 
--imNoDerivNoRefs, )
import Utils.Drasil

import Drasil.Diagnose.Unitals 
import Drasil.Diagnose.GenDefs (genDefns, vLoadtGD)
import Data.Drasil.Concepts.Math (constraint)


iMods :: [InstanceModel]
iMods = [elimConstIM]

elimConstIM :: InstanceModel 
elimConstIM = imNoRefs elimConstRC 
  [qwC vLoadt $ Bounded (Exc, 0) (Exc, sy vLoado)
  ,qwC time $ UpFrom (Exc, 0)]
  (qw elimConst) [UpFrom (Exc, 0)]
  (Just elimConstDeriv) "calofElimConst" [elimConstraintNote]

elimConstRC :: RelationConcept
elimConstRC = makeRC "elimConstRC" (nounPhraseSP "calculation of elimination rate")
  EmptyS $ sy elimConst $= (ln(sy vLoado) - ln(sy vLoadt)) / sy time
  
elimConstDeriv :: Derivation
elimConstDeriv = mkDerivName (phrase elimConst) (weave [elimConstDerivSents, map E elimConstDerivEqns])


elimConstDerivSents :: [Sentence]
elimConstDerivSents = [elimConstDerivSent1, elimConstDerivSent2,
                      elimConstDerivSent3, elimConstDerivSent4]

elimConstDerivSent1, elimConstDerivSent2,elimConstDerivSent3, elimConstDerivSent4 :: Sentence

elimConstDerivSent1 = foldlSentCol [S "Using the relationship for the viral load with respect to time" `sIn` makeRef2S vLoadtGD `sC`
                                 S "we have" ]
                                 
elimConstDerivSent2 = foldlSentCol [S "Where", ch vLoadt +:+ S "denotes the", phrase vLoadt `sC`
                                    ch vLoado +:+ S "denotes the", phrase vLoado `sAnd` ch elimConst +:+ 
                                    S "denotes the", phrase elimConst +:+ 
                                    S ". When isolating for the elimination constant " `sC` S " we have"]
                                    
elimConstDerivSent3 = foldlSentCol [S "To isolate further, the natural logarithm is applied"]
elimConstDerivSent4 = foldlSentCol [S "After using the logarithmic quotient property" `sC` S "we have the required equation"]


elimConstDerivEqns :: [Expr]
elimConstDerivEqns = [elimConstDerivEqn1, elimConstDerivEqn2, elimConstDerivEqn3, elimConstDerivEqn4]

elimConstDerivEqn1, elimConstDerivEqn2, elimConstDerivEqn3, elimConstDerivEqn4 :: Expr
elimConstDerivEqn1 =  sy vLoadt $= sy vLoado * exp (negate(sy elimConst * sy time))
elimConstDerivEqn2 =  sy vLoadt / sy vLoado $= exp (negate(sy elimConst * sy time))
elimConstDerivEqn3 =  ln (sy vLoadt / sy vLoado) $= (negate(sy elimConst * sy time))
elimConstDerivEqn4 =  sy elimConst $= (ln(sy vLoado) - ln(sy vLoadt)) / sy time


---- NOTES

elimConstraintNote :: Sentence

elimConstraintNote = foldlSent [S "The" +:+ phrase constraint +:+
     E ( sy vLoado $> sy vLoadt $> 0) `sIs` S "required for the nature of the problem.",
     S "Due to the input constraint" `sC`
     S " the" +:+ E ( sy elimConst $> 0) `sIs` S "established"]

     
     
     
     
     
     
