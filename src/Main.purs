module Main where

import Prelude

import Control.Monad.Eff as Eff
import Control.Monad.Eff.Console as Eff.Console
import Semigroup as Semigroup
import Semiring as Semiring

main :: ∀ eff. Eff.Eff (console :: Eff.Console.CONSOLE | eff) Unit
main = do
  Eff.Console.log "Semigroup Validation:"
  Semigroup.main
  Eff.Console.log "\n"
  Eff.Console.log "Semiring Validation:"
  Semiring.main
