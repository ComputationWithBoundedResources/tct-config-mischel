{-# LANGUAGE CPP #-}
module Main where

import           Data.Typeable

import           Tct.Core
import qualified Tct.Core.Common.Parser       as P
import qualified Tct.Core.Common.Pretty       as PP
import qualified Tct.Core.Data                as T
import           Tct.Core.Interactive

import           Tct.Trs.Data
import qualified Tct.Trs.Data.DependencyGraph as DG
import qualified Tct.Trs.Data.Mode            as M
import qualified Tct.Trs.Data.Problem         as Prob
import qualified Tct.Trs.Data.RuleSelector    as RS
import qualified Tct.Trs.Data.Trs             as Trs
import           Tct.Trs.Interactive
import           Tct.Trs.Processor

import           Certify
import           DC
import           RC

import qualified Debug.Trace                  as T


main :: IO ()
main = tm `setModeWith`
  defaultTctConfig
#ifdef NOTRECOMPILE
    { recompile = False }
#endif
    -- { defaultSolver = Just ("minismt",["-v2","-m", "-neg", "-ib", "4", "-ob", "6"]) }
    { defaultSolver = Just ("yices-smt2",[]) }


tm :: M.TrsMode
tm = M.trsMode
  `withStrategies`
    [ T.SD certifySD
    , T.SD runtimeSD
    , T.SD derivationalSD ]
  `withDefaultStrategy` T.deflFun competitionSD


competitionSD = strategy "competition" (OneTuple $ some timArg `T.optional` Nothing) competition
  where timArg = nat `withName` "timeout" `withHelp` ["timeout"]

competition mto =
  timeoutRelative mto 100 $ withProblem $ \p ->
    if Prob.isRCProblem p
      then runtime' Best mto
      else derivational


-- trace :: String -> TrsStrategy -> TrsStrategy
-- trace s st = T.trace s $ withProblem $ \ prob ->
--   T.trace s $ T.trace (PP.display $ PP.pretty prob) st

-- timArg = nat `withName` "timeout" `withHelp` ["timeout"]
-- degArg = nat `withName` "degree" `withHelp` ["max degree"]


