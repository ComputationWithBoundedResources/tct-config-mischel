{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where


import           Tct.Core
import qualified Tct.Core.Data                as T

import qualified Tct.Trs.Data.Mode            as M
import qualified Tct.Trs.Data.Problem         as Prob

import           Certify                      
import           RC
import           DC


main :: IO ()
main = tm `setModeWith` 
  defaultTctConfig  { recompile = False }

tm :: M.TrsMode
tm = M.trsMode
  `withStrategies`
    [ T.SD $ certifySD
    , T.SD $ competitionSD ]

competitionSD = strategy "competition" (OneTuple $ some timArg `T.optional` Nothing) competition
  where timArg = nat `withName` "timeout" `withHelp` ["timeout"]

competition mto = 
  timeoutRelative mto 100 $ withProblem $ \p -> 
    if Prob.isRCProblem p
      then runtime Best mto
      else derivational

