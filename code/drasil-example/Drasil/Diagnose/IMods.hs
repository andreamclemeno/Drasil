module Drasil.Diagnose.IMods where

import Prelude hiding (exp)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC) 
import Utils.Drasil

import Drasil.Diagnose.Assumptions
import Drasil.Diagnose.Unitals 
import Drasil.Diagnose.GenDefs (genDefns, vLoadtGD)
import Data.Drasil.Concepts.Math (constraint)
import Drasil.Diagnose.DataDefs
import Drasil.Diagnose.Goals
import Drasil.Diagnose.Requirements
import Drasil.Diagnose.References 


iMods :: [InstanceModel]
iMods = [elimConstIM, predictedVLIM]

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

elimConstDerivSent1 = foldlSentCol [S "Using the relationship for the viral load seen in", makeRef2S viralLoadDD +:+ S "with respect to time" `sIn` makeRef2S vLoadtGD `sC`
                                 S "we have" ]
                                 
elimConstDerivSent2 = foldlSentCol [S "Where", ch vLoadt +:+ S "denotes the", phrase vLoadt `sC`
                                    ch vLoado +:+ S "denotes the", phrase vLoado `sAnd` ch elimConst +:+ 
                                    S "denotes the", phrase elimConst :+:
                                    S ". When isolating for the elimination constant" `sC` S " we have"]
                                    
elimConstDerivSent3 = foldlSentCol [S "To isolate further, the natural logarithm is applied"]
elimConstDerivSent4 = foldlSentCol [S "After using the logarithmic quotient property" `sC` S "we have the required equation"]


elimConstDerivEqns :: [Expr]
elimConstDerivEqns = [elimConstDerivEqn1, elimConstDerivEqn2, elimConstDerivEqn3, elimConstDerivEqn4]

elimConstDerivEqn1, elimConstDerivEqn2, elimConstDerivEqn3, elimConstDerivEqn4 :: Expr
elimConstDerivEqn1 =  sy vLoadt $= sy vLoado * exp (negate(sy elimConst * sy time))
elimConstDerivEqn2 =  sy vLoadt / sy vLoado $= exp (negate(sy elimConst * sy time))
elimConstDerivEqn3 =  ln (sy vLoadt / sy vLoado) $= (negate(sy elimConst * sy time))
elimConstDerivEqn4 =  sy elimConst $= (ln(sy vLoado) - ln(sy vLoadt)) / sy time

   
predictedVLIM :: InstanceModel 
predictedVLIM = imNoRefs predictedVLRC 
  [qwC vLoado $ UpFrom (Exc, 0)
  ,qwC time $ UpFrom (Exc, 0)]
  (qw predictedVL) [Bounded (Exc, 0) (Exc, sy vLoado)]
  (Just predictedVLDeriv) "calofPredictedVL" [predictedVLConstraintNote]

predictedVLRC :: RelationConcept
predictedVLRC = makeRC "predictedVLRC" (nounPhraseSP "calculation of elimination rate")
  EmptyS $ sy predictedVL $= sy vLoado * exp (negate(sy elimConst * sy time))
  
predictedVLDeriv :: Derivation
predictedVLDeriv = mkDerivName (phrase predictedVL) (weave [predictedVLDerivSents, map E predictedVLDerivEqns])


predictedVLDerivSents :: [Sentence]
predictedVLDerivSents = [predictedVLDerivSent1, predictedVLDerivSent2]

predictedVLDerivSent1, predictedVLDerivSent2:: Sentence

predictedVLDerivSent1 = foldlSentCol [S "Using the relationship for the viral load seen in", makeRef2S viralLoadDD +:+ S "with respect to time" `sIn` makeRef2S vLoadtGD `sC`
                                 S "we have" ]
                                 
predictedVLDerivSent2 = foldlSentCol [S "Where", ch vLoadt +:+ S "denotes the", phrase vLoadt `sC`
                                    ch vLoado +:+ S "denotes the", phrase vLoado `sAnd` ch elimConst +:+
                                    S "denotes the", phrase elimConst :+: 
                                    S ". When predicting the viral load after 30 days, the", phrase elimConst +:+ S " found" `sIn` makeRef2S elimConstIM `sC`
                                 S " is used instead of", ch vLoadt +:+ S " therefore we have"]
                                    

predictedVLDerivEqns :: [Expr]
predictedVLDerivEqns = [predictedVLDerivEqn1, predictedVLDerivEqn2]

predictedVLDerivEqn1, predictedVLDerivEqn2 :: Expr
predictedVLDerivEqn1 =  sy vLoadt $= sy vLoado * exp (negate(sy elimConst * sy time))
predictedVLDerivEqn2 =  sy predictedVL $= sy vLoado * exp (negate(sy elimConst * sy time))


---- NOTES

elimConstraintNote :: Sentence
elimConstraintNote = foldlSent [S "The" +:+ phrase constraint +:+
     E ( sy vLoado $> sy vLoadt $> 0) `sIs` S "required for the nature of the problem with respect to"+:+ makeRef2S proportional :+: S ". Due to the input constraint" `sC` S " the" +:+ E ( sy elimConst $> 0) `sIs` S "established due to"  +:+ makeRef2S alwaysElim :+: S ". Using this instance model, the goal of the software in " +:+ makeRef2S detElimrate +:+ S " can be achieved. In addition, this constraint is used to achieve a functional requirement seen in " +:+ makeRef2S verifyOutput]
     
predictedVLConstraintNote :: Sentence

predictedVLConstraintNote = foldlSent [S "The" +:+ phrase constraint +:+
     E ( sy vLoado $> sy vLoadt $> 0) `sIs` S " applies when determining future values from the in initial infection with respect to"+:+ makeRef2S initialInf  +:+ S "as well as " +:+ makeRef2S alwaysElim :+: S ". Using this instance model, the goal of the software in " +:+ makeRef2S predictVL30 +:+ S " can be achieved. In addition, this constraint is used to achieve a functional requirement seen in " +:+ makeRef2S verifyOutput]
     

  