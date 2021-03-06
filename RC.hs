{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module RC
  ( runtime
  , runtime'
  , runtimeSD
  , CombineBy (..)
  ) where

import           Data.Typeable

import           Tct.Core
import qualified Tct.Core.Common.Parser       as P
import qualified Tct.Core.Data                as T

import           Tct.Trs.Data
import qualified Tct.Trs.Data.DependencyGraph as DG
import qualified Tct.Trs.Data.Problem         as Prob
import qualified Tct.Trs.Data.Trs             as Trs
import           Tct.Trs.Processor


timArg = nat `withName` "timeout" `withHelp` ["timeout"]

data CombineBy = Best | Fastest    deriving (Show, Enum, Bounded, Typeable)
instance T.SParsable i i CombineBy where parseS = P.enum

comArg = T.arg
  `withName` "combine"
  `T.withDomain` fmap show [(minBound :: CombineBy) ..]
  `withHelp` ["combine with"]

runtimeSD = strategy "runtime" (comArg `T.optional` Fastest, some timArg `T.optional` Nothing) runtimeS

runtime  = T.deflFun runtimeSD
runtime' = T.declFun runtimeSD


--- * direct ---------------------------------------------------------------------------------------------------------

matchbounds = bounds Minimal Match <||> bounds PerSymbol Match

mx dim deg   = matrix' dim deg Algebraic ?ua ?ur ?sel ?gr
mxCP dim deg = matrixCP' dim deg Algebraic ?ua ?ur

px 1 = poly' Linear Restrict ?ua ?ur ?sel ?gr
px n = poly' (Mixed n) Restrict ?ua ?ur ?sel ?gr

pxCP 1 = polyCP' Linear Restrict ?ua ?ur
pxCP n = polyCP' (Mixed n) Restrict ?ua ?ur

wgOnUsable dim deg = weightgap' dim deg Algebraic ?ua WgOnTrs
wg dim deg         = weightgap' dim deg Algebraic ?ua WgOnAny

--- * rc -------------------------------------------------------------------------------------------------------------

runtimeS :: CombineBy -> Maybe Int -> TrsStrategy
runtimeS combineBy mto =
  let
    ?timeoutRel = timeoutRelative mto
    ?combine    = case combineBy of { Best -> best cmpTimeUB; Fastest -> fastest }
    ?ua         = UArgs
    ?ur         = URules
    ?gr         = NoGreedy
    ?sel        = Just $ selAny
  in

  maybe id timeoutIn mto $ withProblem $ \ prob ->
    if Prob.isInnermostProblem prob
      then rci
      else iteProgress toInnermost rci rc

withDP =
  try (withProblem toDP')
  >>> try removeInapplicable
  >>> try cleanSuffix
  >>> te removeHeads
  >>> te (withProblem partIndep)
  >>> try cleanSuffix
  >>> try trivial
  >>> try usableRules
  where
    toDP' prob
      | Prob.isInnermostProblem prob =
          timeoutIn 5 (dependencyPairs >>> try usableRules >>> wgOnTrs)
          <|> dependencyTuples >>> try usableRules
      | otherwise =
          dependencyPairs >>> try usableRules >>> try wgOnTrs

    partIndep prob
      | Prob.isInnermostProblem prob = decomposeIndependentSG
      | otherwise                    = linearPathAnalysis

    wgOnTrs = withProblem $ \ prob ->
      if (Trs.null $ Prob.strictTrs prob)
        then succeeding
        else wgOnUsable 1 1 <||> wgOnUsable 2 1

trivialDP =
  dependencyTuples
  >>> try usableRules
  >>> try trivial
  >>> try dpsimps
  >>> tew (wg 1 0 <||> mx 1 0)

withCWDG s = withProblem $ \ prob -> s (Prob.congruenceGraph prob)


--- ** rci ----------------------------------------------------------------------------------------------------------

rci =
  try innermostRuleRemoval
  >>! ?combine
    [ named "TRIVIAL" $ timeoutIn 7 $ trivialDP   >>> empty
    , named "BOUNDS"  $ timeoutIn 7 $ matchbounds >>> empty
    , named "SHIFT"   $
      ?combine
        [ named "DIRECT"  $ interpretations >>> empty
        , named "WITHDP"  $ withDP >>!! dpi >>> empty ]
    ]
  where

interpretations =
  tew (?timeoutRel 15 $ mx 1 1 <||> wg 1 1) 
  >>> fastest
    [ named "POLYS"    $ tew (px 2) >>> tew (px 3)
    , named "MATRICES" $ tew (?timeoutRel 15 mxs1) >>> tew (?timeoutRel 15 mxs2) >>> tew mxs3 >>> tew mxs4 ]
  where
    mxs1 = mx 2 1 <||> mx 3 1
    mxs2 = mx 2 2 <||> mx 3 2 <||> wg 2 2
    mxs3 = mx 3 3 <||> mx 4 3
    mxs4 = mx 4 4

dpi =
  tew (withCWDG trans) >>> basics
  where
    trans cwdg
      | cwdgDepth cwdg == (0::Int) = shiftLeafs
      | otherwise                  = ?timeoutRel 25 shiftLeafs <|> removeFirstCongruence

    cwdgDepth cwdg = maximum $ 0 : [ dp r | r <- DG.roots cwdg]
      where dp n = maximum $ 0 : [ 1 + dp m | m <- DG.successors cwdg n]


    shiftLeafs = removeLeafs 0 <||> (removeLeafs 1 <|> removeLeafs 2)

    removeLeafs 0 = tew $ removeLeaf (mxCP 1 0)
    removeLeafs i =
      removeLeaf (mxCP i i)
      <||> removeLeaf (mxCP (i+1) i)
      <||> when (i == 1) (removeLeaf (mxCP 3 1))
      <||> when (i == 2) (removeLeaf (pxCP 2))

    removeFirstCongruence = decomposeDG decomposeDGselect (Just $ proc) Nothing >>! try simps
      where proc = try simps >>> tew shiftLeafs >>> basics >>> empty

    basics = tew shift
      where shift = mx 2 2 <||> mx 3 3 <||> px 3 <||>  mx 4 4

    simps =
      try empty
      >>> cleanSuffix
      >>! try trivial
      >>> try usableRules



--- ** rc ------------------------------------------------------------------------------------------------------------

rc =
  ?combine
    [ named "TRIVIAL" $ timeoutIn 7 $ trivialDP >>> empty
    , named "BOUNDS"  $ timeoutIn 7 $ matchbounds >>> empty
    , named "SHIFT"   $
      ?combine
        [ named "DIRECT"  $ interpretations >>> empty
        , named "WITHDP"  $ withDP >>!! dp >>> empty ]
    ]
  where

  dp = withProblem $ loopFrom 1
    where
      loopFrom i prob
        | Trs.null (Prob.strictTrs prob) = dpi
        | otherwise                      = tew (ints i) >>! withProblem (loopFrom $ succ i)
      ints i =
        let ?sel = Just selAnyRule in
        mx i i
        <||> wg i i
        <||> when (i == 2 || i == 3) (px i)
        <||> when (i < 4) (mx (succ i) i <||> wg (succ i) i)

