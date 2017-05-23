module Drasil.OrganizationOfSRS (refineChain, orgSec, orgSecWTS) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Utils (foldlsC)

-- | Create a list in the pattern of "The __ are refined to the __".
-- Note: Order matters!
refineChain :: NamedIdea c => [c] -> Sentence
refineChain (x:y:[]) = S "The" +:+ word x +:+ S "are refined to the" +:+ word y
refineChain (x:y:xs) = refineChain [x,y] `sC` rc ([y] ++ xs)
refineChain _ = error "refineChain encountered an unexpected empty list"

-- | Helper used by refineChain
word :: NamedIdea c => c -> Sentence
word w = plural $ w ^. term

-- | Helper used by refineChain
rc :: NamedIdea c => [c] -> Sentence
rc (x:y:[]) = S "and the" +:+ (plural $ x ^. term) +:+ S "to the" +:+. 
  (plural $ y ^. term)
rc (x:y:xs) = S "the" +:+ word x +:+ S "to the" +:+ word y `sC` rc ([y] ++ xs)
rc _ = error "refineChain helper encountered an unexpected empty list"

-- | Organization of the document section builder. Takes an introduction,
-- a "bottom" chunk (where to start reading bottom-up. Usually instance
-- models or data definitions), a bottom section (for creating a reference link)
-- which should match the bottom chunk, but does not have to.
orgSec :: (NounPhrase c) => Sentence -> c -> Section -> Section
orgSec = \i b s ->
  Section (titleize orgOfDoc) (map Con (orgIntro i b s Nothing))

-- | Same as 'orgSec' with the addition of extra information at the end 
-- (post-refine chain)?
orgSecWTS :: (NounPhrase c) => Sentence -> c -> Section -> Sentence -> Section
orgSecWTS = \i b s t ->
  Section (titleize orgOfDoc) (map Con (orgIntro i b s (Just t)))
  
-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: (NounPhrase c) => Sentence -> c -> Section -> Maybe Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [ Paragraph $
  intro +:+ S "The presentation follows the standard pattern of presenting" +:+
  (foldlsC $ map plural [goal, theory, definition]) `sC` S "and assumptions." +:+
  S "For readers that would like a more bottom up approach" `sC`
  S "they can start reading the" +:+ (plural bottom) +:+ 
  S "in" +:+ (makeRef bottomSec) +:+. 
  S "and trace back to find any additional information they require",
  Paragraph $ lastS trailingSentence ]
  where lastS Nothing = refineChain [goalStmt, thModel, inModel]
        lastS (Just t) = lastS Nothing +:+. t