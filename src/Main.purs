module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import VexFlow as VX

foreign import data VEXFLOW1 :: !

main :: forall e. Eff (vexFlow :: VX.VEXFLOW | e) Unit
main = do
  canvas <- VX.createCanvas "one canvas"
  VX.createRenderer canvas
