{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import           RC
import           DC

import qualified Debug.Trace                  as T


main :: IO ()
main = tm `setModeWith` 
  defaultTctConfig
    -- { defaultSolver = Just ("minismt",["-v2","-m", "-neg", "-ib", "4", "-ob", "6"]) }
    -- { defaultSolver = Just ("z3",["-smt2","-in"]) }


tm :: M.TrsMode
tm = M.trsMode
  `withStrategies`
    [ T.SD $ certifySD
    , T.SD $ runtimeSD
    , T.SD $ dcfSD
    , T.SD $ derivationalSD ]
  `withDefaultStrategy` (T.deflFun runtimeSD)

-- trace :: String -> TrsStrategy -> TrsStrategy
-- trace s st = T.trace s $ withProblem $ \ prob ->
--   T.trace s $ T.trace (PP.display $ PP.pretty prob) st

-- timArg = nat `withName` "timeout" `withHelp` ["timeout"]
-- degArg = nat `withName` "degree" `withHelp` ["max degree"]


