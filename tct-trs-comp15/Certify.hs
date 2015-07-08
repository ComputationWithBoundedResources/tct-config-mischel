{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Certify (certify, certifySD) where

import           Tct.Core

import qualified Tct.Trs.Data.Problem as Prob
import qualified Tct.Trs.Data.Trs     as Trs
import           Tct.Trs.Processor


degArg = nat `withName` "degree" `withHelp` ["max degree"]

certifySD = strategy "certify" (OneTuple $ degArg `optional` 5) certify

certify deg = withProblem certify' where
  certify' prob
    | isRC && isIn = let ?ua = UArgs in certifyRCI deg
    | isRC         = let ?ua = NoUargs in certifyRC deg
    | otherwise    = certifyDC deg
    where
      isRC = Prob.isRCProblem prob
      isIn = Prob.isInnermostProblem prob


matchbounds = withProblem $ \prob ->
  when (Trs.isLeftLinear $ Prob.allComponents prob) (timeoutIn 8 $ bounds PerSymbol Match)

shifts l u = chain [ tew (ints d) | d <- [(max 0 l) .. u] ]

ints 2 = mx 2 <||> px 2
ints 3 = mx 3 <||> px 3
ints n = mx n

mx d = matrix' d d Triangular ?ua URules (Just selAny) NoGreedy
px d = poly' (Mixed d) Restrict ?ua URules (Just selAny) NoGreedy


certifyRC deg = matchbounds <||> interpretations where
  interpretations =
    shifts 1 1
    >>! fastest
      [ dependencyPairs' WDP >>> try usableRules >>> shifts 1 deg >>> empty
      , shifts 2 2
        >>! fastest
          [ dependencyPairs' WDP >>> try usableRules >>> shifts 1 deg >>> empty
          ,                                              shifts 3 deg >>> empty ]
      ]


certifyRCI deg =
  try innermostRuleRemoval
  >>! (matchbounds <||> interpretations)
  where
    interpretations =
      shifts 1 1
      >>! fastest
        [ dependencyTuples     >>> try usableRules >>> shifts 1 deg >>> empty
        , dependencyPairs' WDP >>> try usableRules >>> shifts 1 deg >>> empty
        ,                                              shifts 2 deg >>> empty ]

      <|>

      shifts 1 1 >>! shifts 2 2
      >>! fastest
        [ dependencyTuples     >>> try usableRules >>> shifts 1 deg >>> empty
        , dependencyPairs' WDP >>> try usableRules >>> shifts 1 deg >>> empty
        ,                                              shifts 3 deg >>> empty ]


certifyDC deg =
  try innermostRuleRemoval
  >>! (matchbounds <||> interpretations 1 deg)
  where
    interpretations l u = chain [ tew (mxAny d) | d <- [(max 0 l) .. (min u deg)] ]
    mxAny d = matrix' d d Triangular NoUargs NoURules (Just selAny) NoGreedy
    -- mxAll d = matrix' d d Triangular NoUargs NoURules (Just sel) NoGreedy

