{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#!/usr/bin/runhaskell

import           Tct.Core

import qualified Tct.Core.Common.Parser  as TP
import qualified Tct.Core.Data           as T
import qualified Tct.Core.Parse          as TP

import qualified Tct.Trs                 as R
import qualified Tct.Trs.Processor       as R

import qualified Tct.Its                 as I
import qualified Tct.Its.Processor       as I

import           Its                     (defaultSD)
import           RC                      (runtimeSD)


import           Tct.Jbc.Data

import           Tct.Jbc
import           Tct.Jbc.Data.Mode       (JbcMode)
import           Tct.Jbc.Processor

main :: IO ()
main = jm `setModeWith` defaultTctConfig

jm :: JbcMode
jm = jbcMode
  `withStrategies`
    [ T.SD jatDeclaration
    , T.SD trsDeclaration
    , T.SD itsDeclaration ]
  `withDefaultStrategy` (T.deflFun jatDeclaration)


jatDeclaration :: T.Declaration (
  '[ T.Argument 'T.Optional (Strategy R.TrsProblem R.TrsProblem)
  ,  T.Argument 'T.Optional (Strategy I.Its I.Its) ]
  T.:-> JbcStrategy)
jatDeclaration = T.strategy "jat" (trsArg `T.optional` trs, itsArg `T.optional` its) jatStrategy

-- MS: dependencies between modules are annoying to handle here we would like the declaration list of the
-- configuration files but the configuration files are no Main modules; we would have to define the list of
-- declarations in a sparate module 
instance T.SParsable Jbc Jbc (Strategy R.TrsProblem R.TrsProblem) where
  parseS = TP.withState ds TP.strategy
    where ds = T.SD runtimeSD : R.defaultDeclarations

instance T.SParsable Jbc Jbc (Strategy I.Its I.Its) where
  parseS = TP.withState ds TP.strategy
    where ds = T.SD defaultSD : I.defaultSDs


--- * its ------------------------------------------------------------------------------------------------------------

itsDeclaration :: T.Declaration ('[ T.Argument 'T.Optional (Strategy I.Its I.Its) ] T.:-> JbcStrategy)
itsDeclaration = T.strategy "its" (OneTuple $ itsArg `T.optional` its) itsStrategy

its :: Strategy I.Its I.Its
its = T.deflFun defaultSD


--- * trs ------------------------------------------------------------------------------------------------------------

trsDeclaration :: T.Declaration (
  '[Argument 'Optional Narrow
  , Argument 'Optional (T.Strategy R.TrsProblem R.TrsProblem)]
  T.:-> T.Strategy Jbc Jbc)
trsDeclaration = T.strategy "trs" (narrowArg `T.optional` Narrow, trsArg `T.optional` trs) trsStrategy

trs :: T.Strategy R.TrsProblem R.TrsProblem
trs =
  R.dependencyPairs
  >>> try R.usableRules
  >>> try R.dpsimps
  >>> try R.cleanSuffix
  >>> try R.dpsimps
  >>> R.ints (Just 0) 4

