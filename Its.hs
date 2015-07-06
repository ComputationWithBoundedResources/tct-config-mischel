{-# LANGUAGE ImplicitParams #-}
module Its where

import qualified Data.IntMap.Strict         as IM

import           Tct.Core

import           Tct.Its
import           Tct.Its.Data
import           Tct.Its.Processor


im :: TctMode Its Its ()
im = itsMode
  `withStrategies` [ SD simpleSD ]
  `withDefaultStrategy` timeoutIn 30 (simple False False)

simpleSD = strategy "simple" (atarg, afarg) simple where
  atarg = bool `withName` "useTransitionAbstraction" `optional` False
  afarg = bool `withName` "useArgumentFilter" `optional` False

simple :: Bool -> Bool -> ItsStrategy
simple useAT useAF =
  let
    ?nInChain  = 5  :: Int
    ?nOutChain = 15 :: Int
    ?useAT    = useAT
    ?useAF    = useAF
  in

  wellformed
  >>> try simpl1
  >>> try (when ?useAT (withProblem (transitionAbstraction . monotonicityPredicates)))
  >>> try (when ?useAF (withProblem (argumentFilter . unusedFilter)))
  -- >>> try pathAnalysis -- update rvgraph error we have to be more careful with init/update; or just compute it
  -- instead of throwing an error in updateSizebounds
  >>> try st
  >>> withChaining st
  >>> (withProblem $ \prob -> if isClosed prob then succeeding else failing' "not closed")
  where
    st =
      try simpl2
      >>> te (withKnowledgePropagation farkas)
      >>> te (try sizebounds >>> usingTimebounds)
    usingTimebounds = withProblem $
      \prob -> es $ fastestN 8 [ withKnowledgePropagation (timebounds c) | c <- timeboundsCandidates (selNextSCC prob) ]


wellformed :: ItsStrategy
wellformed = withProblem
  $ \prob -> if validate (IM.elems $ _irules prob) then succeeding else failing' "The problem is malformed."

-- FIXME: boundtrivialsccs is not always 1 in the recursive case;
-- l1 -> c(l2,l2)
-- l2 -> l3
-- l3 -> l4; take max label ; and propagate information
simpl1 :: ItsStrategy
simpl1 = force $
  try boundTrivialSCCs
  >>> try unsatRules

simpl2 :: ItsStrategy
simpl2 = force $
  try unsatPaths
  >>> try unreachableRules
  >>> try leafRules

withArgumentFilter :: ItsStrategy -> ItsStrategy
withArgumentFilter st = st >>> try af
  where af = withProblem (argumentFilter . unusedFilter)

withKnowledgePropagation :: ItsStrategy -> ItsStrategy
withKnowledgePropagation st = st >>> try knowledgePropagation

innerChaining :: ItsStrategy
innerChaining = withProblem $ \prob -> chaining . chainingCandidates k prob $ selNextSCC prob
  where k prob r = maxCost 10 prob r && maxOuts 2 prob r

-- choice1.koat 6 is fine 7 not is there some refinement step in st causing the loop
outerChaining :: ItsStrategy
outerChaining = withProblem $ \prob -> chaining . chainingCandidates k prob $ selToNextSCC prob
  where k prob r = maxCost 20 prob r && maxOuts 2 prob r

withChaining :: (?nInChain :: Int, ?nOutChain :: Int) => ItsStrategy -> ItsStrategy
withChaining st = es $ try st >>> (exhaustivelyN ?nInChain innerChaining <|> exhaustivelyN ?nOutChain outerChaining)
-- withChaining st = try st >>> (exhaustivelyN ?nInChain innerChaining <> exhaustivelyN ?nOutChain outerChaining) >>> try st

-- chainN n s = s >>> (chain $ replicate n (try s))

