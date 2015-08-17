module DC (derivational, derivational', derivationalSD, dcfSD) where
import qualified Data.Set                  as S (fold)

import           Tct.Core
import           Tct.Core.Data             as T (declFun, deflFun)

import           Tct.Trs.Data.Problem      (signature, trsComponents)
import           Tct.Trs.Data.RuleSelector (selAllOf, selStricts)
import           Tct.Trs.Data.Signature    (arity, symbols)
import           Tct.Trs.Data.Trs          (isNonSizeIncreasing)
import           Tct.Trs.Processor

-- MS:
-- decomposition after shifting does not work (as composition basically searches for an interpretation)
-- matchbounds sometimes/rarely applies after shifting rules to the weak component or after decomposition
-- SRS benefit from higher constants

derivationalSD = strategy "derivational" () dc
derivational   = T.deflFun derivationalSD
derivational'  = T.declFun derivationalSD

dcfSD = strategy "dcf" () dcfast


iteNonSizeIncreasing st1 st2 = withProblem $ \prob ->
  if isNonSizeIncreasing (trsComponents prob) then st1 else st2

isSRS prob = S.fold (\sym -> (arity sig sym == 1 &&)) True (symbols sig)
  where sig = signature prob

iteSRS st1 st2 = withProblem $ \prob ->
  if isSRS prob then st1 else st2

whenSRS st = withProblem $ \prob -> when (isSRS prob) st

compose =
  iteNonSizeIncreasing
    (mul 1 <|> mul 2 <|> (mul 3 <||> mul 4))
    (com 1 <|> com 2 <|> (com 3 <||> com 4))

matchbounds = bounds Minimal Match <||> bounds PerSymbol Match

mx dim deg  = matrix' dim deg Algebraic NoUArgs NoURules (Just selAny) NoGreedy
mx' dim deg = matrix' dim deg Algebraic NoUArgs NoURules Nothing NoGreedy
wg dim deg  = weightgap' dim deg Algebraic NoUArgs WgOnAny

mxCP dim deg = matrixCP' dim deg Algebraic NoUArgs NoURules
mul n = decomposeCP' selAny RelativeMul  (mxCP n n)
com n = decomposeCP' selAny RelativeComp (mxCP n n)

mxs 1 = mx 1 1 <||> mx 2 1 <||> mx 3 1 <||> wg 2 1 <||> wg 1 1
mxs 2 = mx 2 2 <||> mx 3 2 <||> mx 4 2 <||> wg 2 2
mxs 3 = mx 3 3 <||> mx 4 3 <||> wg 3 3
mxs 4 = mx 4 4 <||> wg 4 4
mxs n
  | n > 0     = mx n n
  | otherwise = mx 1 1


dc =
  best cmpTimeUB
    [ timeoutIn 20 matchbounds
    , interpretation1
    , composition1 ]

  where

  ideg = 4
  bdeg = max ideg 6

  ints = chain [ tew (mxs d) | d <- [1..ideg] ]

  -- basics1 = fastest $ timeoutIn 10 matchbounds : [mx' i i | i <- [1..bdeg]]
  basics2 = fastest [mx' i i | i <- [succ ideg..bdeg]]

  interpretation1 = force ints    >>! composition2
  composition1    = force compose >>> (interpretation2 <||> composition2)

  interpretation2 = try ints >>! composition2
  composition2    = iteProgress compose (interpretation2 <||> composition2) basics2


dcfast = -- withMini $ 
  combine
    [ timeoutIn 25 matchbounds
    , whenSRS $ withMini $ tew (mx 1 1) >>> tew (mx 2 2)
    , interpretations >>> basics
    , composition ]
  where


  withMini = withSolver "minismt" ["-m", "-v2", "-neg", "-ib", "8", "-ob", "10"]
  combine = best cmpTimeUB
  ideg    = 4
  mdeg    = 6

  basics          = fastest $ timeoutIn 5 matchbounds : [ mx' d d | d <- [succ ideg .. mdeg] ]
  -- interpretations = tew . fastest $ [ mx d d | d <- [1 .. ideg] ] ++ [ wg d d | d <- [1 .. ideg] ] -- ++ [ whenSRS (withMini $ mx 1 1 <||> mx 2 2) ]
  interpretations = matrices (Just 1) ideg
  composition     = compose >>! combine [ interpretations >>> basics , composition ]

