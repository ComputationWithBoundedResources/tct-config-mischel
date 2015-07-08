module DC (derivational, derivational', derivationalSD) where

import Tct.Core
import Tct.Core.Data     as T (declFun, deflFun)

import Tct.Trs.Processor


degArg = nat `withName` "degree" `withHelp` ["max degree"]

derivationalSD = strategy "derivational" (OneTuple $ degArg `optional` 10) dc
derivational   = T.deflFun derivationalSD
derivational'  = T.declFun derivationalSD

dc deg = timeoutIn 10 matchbounds <||> interpretations 1 deg
  where

  matchbounds = bounds Minimal Match <||> bounds PerSymbol Match
  interpretations l u = chain [ tew (mxs d) | d <- [(max 0 l) .. u] ]

  mx dim deg = matrix' dim deg Algebraic NoUargs NoURules (Just selAny) NoGreedy
  wg dim deg = weightgap' dim deg Algebraic NoUargs WgOnAny

  mxs 1 = mx 1 1 <||> mx 2 1 <||> mx 3 1 <||> wg 2 1 <||> wg 1 1
  mxs 2 = mx 2 2 <||> mx 3 2 <||> mx 4 2 <||> wg 2 2
  mxs 3 = mx 3 3 <||> mx 4 4 <||> wg 3 3
  mxs 4 = mx 4 4 <||> wg 4 4
  mxs n
    | n > 0 = mx n n
    | otherwise = failing

