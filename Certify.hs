module Certify
  (certify, certifySD)
  where


import           Tct.Core

import qualified Tct.Trs.Data.Problem as Prob
import qualified Tct.Trs.Data.Trs     as Trs
import           Tct.Trs.Processor


degArg = nat `withName` "degree" `withHelp` ["max degree"]

certifySD = strategy "certify" (OneTuple $ degArg `optional` 10) certify

certify deg = withProblem k where
  k p =
    try innermostRuleRemoval
    >>! (matchbounds <||> interpretations)
    where

      matchbounds = withProblem $ \prob ->
        when (Trs.isLeftLinear $ Prob.allComponents prob) (timeoutIn 7 $ bounds PerSymbol Match)

      interpretations =
        shifts 1 1
        >>! fastest
          [ dependencyTuples     >>> try usableRules >>> shifts 1 deg >>> empty
          , dependencyPairs' WDP >>> try usableRules >>> shifts 1 deg >>> empty
          , shifts 2 deg >>> empty ]
        where

          shifts l u = chain [ tew (ints d) | d <- [(max 0 l) .. (min u deg)] ]

          ints 2 = mx 2 <||> px 2
          ints 3 = mx 3 <||> px 3
          ints n = mx n

          ua = if Prob.isInnermostProblem p then UArgs else NoUargs
          ur = URules
          sl = Just selAny
          gr = NoGreedy

          mx d = matrix' d d Triangular ua ur sl gr
          px d = poly' (Mixed d) Restrict ua ur sl gr

