{-# LANGUAGE ImplicitParams #-}
module Its where

import qualified Data.IntMap.Strict as IM

import           Tct.Core
import qualified Tct.Core.Data      as T

import           Tct.Its
import           Tct.Its.Data
import           Tct.Its.Processor


im :: TctMode Its Its ()
im = itsMode
  `withStrategies` [ SD defaultSD ]
  `withDefaultStrategy` timeoutIn 60 (T.deflFun defaultSD)

defaultSD = strategy "default" (atarg, afarg) def where
  atarg = bool `withName` "useTransitionAbstraction" `optional` False
  afarg = bool `withName` "useArgumentFilter"        `optional` False

def :: Bool -> Bool -> ItsStrategy
def useAT useAF =
  let
    ?maxChain  = 2 :: Int
    ?nInChain  = 5 :: Int
    ?nOutChain = 10 :: Int
    ?useAT    = useAT
    ?useAF    = useAF
  in

  wellformed
  >>> try simpl1
  >>> try (when ?useAT (withProblem (transitionAbstraction . monotonicityPredicates)))
  >>> try (when ?useAF (withProblem (argumentFilter . unusedFilter)))
  -- >>> try pathAnalysis -- FIXME: update rvgraph error; just re-compute it
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

-- FIXME: boundtrivialsccs is not always 1 in the recursive case; take max label
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
  where k prob r = maxCost 2 prob r && maxOuts 3 prob r

outerChaining :: ItsStrategy
outerChaining = withProblem $ \prob -> chaining . chainingCandidates k prob $ selToNextSCC prob
  where k prob r = isUnknown prob r && maxCost 20 prob r && maxOuts 4 prob r

withChaining :: (?maxChain :: Int, ?nInChain :: Int, ?nOutChain :: Int) => ItsStrategy -> ItsStrategy
-- withChaining st = es $ try st >>> (exhaustivelyN ?nInChain innerChaining <|> exhaustivelyN ?nOutChain outerChaining)
withChaining st = exhaustivelyN ?maxChain  $ try st >>> (exhaustivelyN ?nInChain innerChaining <|> exhaustivelyN ?nOutChain outerChaining) >>> try empty

