{-# LANGUAGE ImplicitParams #-}
#!/usr/bin/runhaskell

import           Tct.Core

import           Its

main :: IO ()
main = im `setModeWith` defaultTctConfig

